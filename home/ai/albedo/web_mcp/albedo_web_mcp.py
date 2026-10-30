#!/usr/bin/env python3
"""Albedo MCP stdio server: searxng search + page fetching.

Speaks newline-delimited JSON-RPC 2.0 (Model Context Protocol, stdio
transport) and exposes two tools:

  search(query, ...)  local SearXNG instance, JSON API
  fetch(url, ...)     raw response text or trafilatura extraction

Trafilatura is required for extracted fetch mode; raw mode uses only the stdlib.
Everything written to stdout is protocol; logs go to stderr (albedo discards
server stderr).
"""

import json
import os
import sys
import urllib.parse
import urllib.request

import trafilatura

SEARXNG = os.environ.get("SEARXNG_URL", "http://127.0.0.1:8080")
SEARCH_TIMEOUT = 20
MAX_DOWNLOAD = 5_000_000
PROTOCOL_VERSIONS = ("2025-06-18", "2025-03-26", "2024-11-05")
SERVER_INFO = {"name": "albedo-web-mcp", "version": "0.1.0"}

FETCH_UA = "Mozilla/5.0 (X11; Linux x86_64; rv:140.0) Gecko/20100101 Firefox/140.0"


class ToolError(Exception):
    pass


def to_int(args, key, default, lo, hi):
    value = args.get(key, default)
    if value is None:
        return default
    value = int(value)
    if not lo <= value <= hi:
        raise ToolError(f"{key} must be between {lo} and {hi}")
    return value


def search(args):
    query = str(args.get("query", "")).strip()
    if not query:
        raise ToolError("query must not be empty")
    limit = to_int(args, "max_results", 6, 1, 20)
    page = to_int(args, "page", 1, 1, 100)

    params = {"q": query, "format": "json", "pageno": page}
    for arg, param in (("language", "language"), ("engines", "engines"), ("time_range", "time_range")):
        if args.get(arg):
            params[param] = args[arg]
    if args.get("category"):
        params["categories"] = args["category"]

    url = SEARXNG.rstrip("/") + "/search?" + urllib.parse.urlencode(params)
    request = urllib.request.Request(url, headers={"User-Agent": "albedo-web-mcp/0.1"})
    try:
        with urllib.request.urlopen(request, timeout=SEARCH_TIMEOUT) as response:
            data = json.load(response)
    except Exception as error:
        raise ToolError(f"searxng request failed ({url}): {error}") from error

    lines = []
    answers = [a.get("answer", "") if isinstance(a, dict) else str(a) for a in data.get("answers") or []]
    answers = [a for a in answers if a]
    if answers:
        lines.append("Answers: " + "; ".join(answers[:3]))
    for rank, result in enumerate(data.get("results", [])[:limit], 1):
        title = result.get("title") or "(untitled)"
        link = result.get("url") or ""
        snippet = result.get("content") or ""
        lines.append(f"{rank}. {title}\n   {link}\n   {snippet}")
    if not lines:
        return "No results."
    return "\n\n".join(lines)


def fetch(args):
    url = str(args.get("url", "")).strip()
    if not url.lower().startswith(("http://", "https://")):
        raise ToolError("url must be http or https")
    mode = args.get("mode") or "extract"
    if mode not in ("extract", "raw"):
        raise ToolError("mode must be 'extract' or 'raw'")
    fmt = args.get("format") or "markdown"
    if fmt not in ("markdown", "text"):
        raise ToolError("format must be 'markdown' or 'text'")
    max_chars = to_int(args, "max_chars", 20_000, 1, 100_000)
    timeout = to_int(args, "timeout", 30, 1, 120)

    request = urllib.request.Request(
        url,
        headers={
            "User-Agent": FETCH_UA,
            "Accept": "text/html,application/xhtml+xml,*/*;q=0.8",
        },
    )
    try:
        with urllib.request.urlopen(request, timeout=timeout) as response:
            body = response.read(MAX_DOWNLOAD)
            final_url = response.geturl()
    except Exception as error:
        raise ToolError(f"download failed ({url}): {error}") from error

    if mode == "raw":
        text = body.decode("utf-8", errors="replace")
        truncated = len(text) > max_chars
        if truncated:
            text = text[:max_chars].rstrip() + "\n\n[truncated]"
        return f"Raw response from {final_url} ({len(text)} chars{', truncated' if truncated else ''}):\n\n{text}"

    try:
        text = trafilatura.extract(
            body,
            url=final_url,
            output_format="markdown" if fmt == "markdown" else "txt",
            include_links=fmt == "markdown",
            include_tables=True,
            include_comments=False,
            favor_recall=True,
        )
    except Exception as error:
        raise ToolError(f"extraction failed ({final_url}): {error}") from error
    if not text:
        raise ToolError(
            f"no content extracted from {final_url} (page may be JS-rendered, empty, or not an article)"
        )
    truncated = len(text) > max_chars
    if truncated:
        text = text[:max_chars].rstrip() + "\n\n[truncated]"
    return f"Extracted from {final_url} ({len(text)} chars{', truncated' if truncated else ''}):\n\n{text}"


SEARCH_SCHEMA = {
    "type": "object",
    "properties": {
        "query": {"type": "string", "minLength": 1, "description": "The search query"},
        "max_results": {"type": "integer", "minimum": 1, "maximum": 20, "description": "Maximum results to return (default 6, max 20)"},
        "page": {"type": "integer", "minimum": 1, "description": "Result page number, starting at 1"},
        "language": {"type": "string", "description": "Language filter, e.g. 'en', 'de' (default: all languages)"},
        "engines": {"type": "string", "description": "Comma-separated engine list, e.g. 'google,duckduckgo' (default: all enabled engines)"},
        "time_range": {"type": "string", "enum": ["day", "week", "month", "year"], "description": "Restrict results to a time range"},
        "category": {"type": "string", "description": "Search category, e.g. 'general', 'it', 'science' (default 'general')"},
    },
    "required": ["query"],
}

FETCH_SCHEMA = {
    "type": "object",
    "properties": {
        "url": {"type": "string", "pattern": "^https?://", "description": "The HTTP or HTTPS URL to fetch"},
        "mode": {"type": "string", "enum": ["extract", "raw"], "description": "'extract' removes boilerplate with trafilatura (default); 'raw' returns the response body as decoded text without extraction"},
        "format": {"type": "string", "enum": ["markdown", "text"], "description": "Extracted content format (default: markdown; ignored in raw mode)"},
        "max_chars": {"type": "integer", "minimum": 1, "maximum": 100000, "description": "Maximum returned characters (default 20000, max 100000)"},
        "timeout": {"type": "integer", "minimum": 1, "maximum": 120, "description": "Timeout in seconds (default 30, max 120)"},
    },
    "required": ["url"],
}

TOOLS = [
    {
        "name": "search",
        "description": "Search the web with the local SearXNG instance. "
        "Returns numbered results with title, URL, and snippet. Pair with fetch to read a result's page.",
        "inputSchema": SEARCH_SCHEMA,
    },
    {
        "name": "fetch",
        "description": "Fetch a web page. By default extracts its main content with trafilatura, removing navigation and "
        "other boilerplate; set mode='raw' to return the response body as decoded text without extraction. "
        "Extracted output is Markdown with links by default, or plain text. Does not execute JavaScript or bypass paywalls.",
        "inputSchema": FETCH_SCHEMA,
    },
]

TOOLS_BY_NAME = {"search": search, "fetch": fetch}


def reply(rid, result):
    return {"jsonrpc": "2.0", "id": rid, "result": result}


def rpc_error(rid, code, message):
    return {"jsonrpc": "2.0", "id": rid, "error": {"code": code, "message": message}}


def text_content(text, is_error=False):
    result = {"content": [{"type": "text", "text": text}]}
    if is_error:
        result["isError"] = True
    return result


def handle(msg):
    # Notifications carry no id and are never answered, even on error.
    if "id" not in msg:
        return None
    rid = msg["id"]
    method = msg.get("method")
    if method == "initialize":
        requested = (msg.get("params") or {}).get("protocolVersion")
        version = requested if requested in PROTOCOL_VERSIONS else PROTOCOL_VERSIONS[-1]
        return reply(
            rid,
            {
                "protocolVersion": version,
                "capabilities": {"tools": {"listChanged": False}},
                "serverInfo": SERVER_INFO,
            },
        )
    if method == "ping":
        return reply(rid, {})
    if method == "tools/list":
        return reply(rid, {"tools": TOOLS})
    if method == "tools/call":
        params = msg.get("params") or {}
        name = params.get("name")
        tool = TOOLS_BY_NAME.get(name)
        if tool is None:
            return rpc_error(rid, -32602, f"unknown tool: {name}")
        args = params.get("arguments")
        if not isinstance(args, dict):
            args = {}
        try:
            return reply(rid, text_content(tool(args)))
        except ToolError as error:
            return reply(rid, text_content(str(error), is_error=True))
        except Exception as error:  # noqa: BLE001 - report, never die
            return reply(rid, text_content(f"{type(error).__name__}: {error}", is_error=True))
    return rpc_error(rid, -32601, f"method not found: {method}")


def main():
    print(f"albedo-web-mcp {SERVER_INFO['version']} ready (searxng: {SEARXNG})", file=sys.stderr)
    for raw in sys.stdin.buffer:
        raw = raw.strip()
        if not raw:
            continue
        try:
            msg = json.loads(raw)
        except ValueError:
            out = rpc_error(None, -32700, "parse error")
        else:
            try:
                out = handle(msg)
            except Exception as error:  # noqa: BLE001 - report, never die
                rid = msg.get("id") if isinstance(msg, dict) else None
                out = None if rid is None else rpc_error(rid, -32603, f"internal error: {error}")
        if out is not None:
            sys.stdout.write(json.dumps(out) + "\n")
            sys.stdout.flush()


if __name__ == "__main__":
    main()
