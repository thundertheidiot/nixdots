---
name: playwright-mcp
description: Drive Helium through the official Playwright MCP browser extension, using existing tabs and logins. Use when interacting with pages in the user's Helium window.
---

# Playwright MCP with Helium

The `playwright` MCP server runs official `@playwright/mcp` in extension mode against the Helium browser supplied by the Nix configuration. It targets the user's normal Helium profile rather than an isolated automation profile. Extension mode may open the normal Helium browser/connect page if it is not already running; it does not reset or migrate a profile.

## Use

Use the `mcp.playwright` tools and observe available tool names before acting. You may open agent-owned tabs when useful; never close user tabs. Request approval before consequential actions (submitting, purchasing, deleting, or changing account settings). Extension mode reduces WebDriver signals but does not guarantee stealth or bypass site protections.

## Working with pages

- `browser_navigate` is the entry point; extension mode reuses the existing Helium window. A leftover `about:blank` tab means the connection is up.
- Prefer `browser_find` (text or regex) to locate elements: it returns refs and context instead of dumping a full accessibility snapshot. Use `browser_snapshot` only when you need the whole tree.
- For targeted reads (values, counts, text), `browser_evaluate` beats snapshotting. `browser_take_screenshot` without `filename` saves to `.cache/playwright-mcp/` and reports the path.
- Large snapshot outputs are saved to `.cache/playwright-mcp/*.yml` with only the path returned inline; read the file if needed.
- To scroll the user's view to a section, click its table-of-contents/anchor link rather than synthesizing scroll events.
- Tabs opened by the agent (`browser_tabs new`) are yours to close; user tabs are not.
- `browser_tabs new` with `about:blank` drops the extension connection; always pass a real URL.
- Connecting, `browser_tabs new` and `browser_tabs select` raise the Helium window if the compositor lets it take focus; `browser_navigate`, `browser_evaluate` and the rest don't. Prefer staying in one tab and navigating.

## Parallel agents

One MCP server has a single "current tab" shared by every caller, so parallel agents on the shared `playwright` server hijack each other's tabs. Each server process gets its own extension connection and tab group though.

- In Claude Code, spawn `browser` subagents (`subagent_type: "browser"`, any model) for web work, one per parallel task. Each starts its own playwright-mcp with a separate tab group.
- Don't use the shared `playwright` tools yourself while `browser` agents are running.
- In albedo, every child or peer agent is its own session with its own MCP connections, so each already gets a separate playwright-mcp and tab group; just spawn children for parallel web work.
