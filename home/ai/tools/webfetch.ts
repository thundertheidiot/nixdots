import { execFile } from "node:child_process"
import { promisify } from "node:util"
import { tool } from "@opencode-ai/plugin"
import type { ToolContext } from "@opencode-ai/plugin"

const exec = promisify(execFile)

const fetchArgs = tool.schema.object({
  url: tool.schema.string().url().regex(/^https?:\/\//i).describe("The HTTP or HTTPS URL to fetch"),
  format: tool.schema
    .enum(["markdown", "text"])
    .optional()
    .describe("Extracted content format (default: markdown)"),
  max_chars: tool.schema
    .number()
    .int()
    .min(1)
    .max(100_000)
    .optional()
    .describe("Maximum extracted characters to return (default 20000, max 100000)"),
  timeout: tool.schema
    .number()
    .int()
    .min(1)
    .max(120)
    .optional()
    .describe("Timeout in seconds, including extraction (default 30, max 120)"),
})

type FetchArgs = ReturnType<typeof fetchArgs.parse>

export default tool({
  description:
    "Fetch a web page and extract its main content using Trafilatura, removing navigation and other boilerplate. " +
    "Returns Markdown with links by default, or plain text. Use to read URLs found by search. " +
    "Does not execute JavaScript or bypass paywalls; not intended for PDFs or binary downloads.",
  args: fetchArgs.shape,
  execute: async (args: FetchArgs, ctx: ToolContext) => {
    await ctx.ask({
      permission: "webfetch",
      patterns: [args.url],
      always: ["*"],
      metadata: { url: args.url },
    })

    const seconds = args.timeout ?? 30
    const timeout = AbortSignal.timeout(seconds * 1000)
    const signal = AbortSignal.any([ctx.abort, timeout])
    const format = args.format ?? "markdown"
    const command = ["--URL", args.url, "--output-format", format === "text" ? "txt" : "markdown", "--no-comments"]
    if (format === "markdown") command.push("--links", "--with-metadata")

    let content: string
    try {
      const { stdout } = await exec("trafilatura", command, {
        signal,
        encoding: "utf8",
        maxBuffer: 5 * 1024 * 1024,
      })
      content = stdout.trim()
    } catch (e) {
      if (ctx.abort.aborted) throw ctx.abort.reason
      if (timeout.aborted) return `Web fetch timed out after ${seconds} seconds: ${args.url}`
      if (e instanceof Error && "code" in e && e.code === "ENOENT") {
        return "Trafilatura is not available. Add python3Packages.trafilatura to programs.opencode.extraPackages."
      }
      return `Web fetch failed: ${(e instanceof Error ? e.message : String(e)).slice(0, 1000)}`
    }

    if (!content) {
      return `No content could be extracted from ${args.url}. The page may be unavailable, blocked, or require JavaScript.`
    }

    const max = args.max_chars ?? 20_000
    const truncated = content.length > max
    return {
      title: `webfetch: ${args.url}`,
      output: `URL: ${args.url}\n\n${content.slice(0, max)}${truncated ? `\n\n[Truncated: showing ${max} of ${content.length} characters. Increase max_chars to read more.]` : ""}`,
      metadata: { url: args.url, format, characters: content.length, shown: Math.min(content.length, max), truncated },
    }
  },
})
