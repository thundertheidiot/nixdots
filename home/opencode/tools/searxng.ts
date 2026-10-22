import { tool } from "@opencode-ai/plugin"
import type { Plugin, ToolContext } from "@opencode-ai/plugin"

const SEARXNG = "http://127.0.0.1:8080"
const TIMEOUT_MS = 20_000

const searchArgs = tool.schema.object({
  query: tool.schema.string().trim().min(1).describe("The search query"),
  max_results: tool.schema
    .number()
    .int()
    .min(1)
    .max(20)
    .optional()
    .describe("Maximum number of results to return (default 6, max 20)"),
  page: tool.schema.number().int().min(1).optional().describe("Result page number starting at 1 (default 1)"),
  language: tool.schema
    .string()
    .optional()
    .describe("Language filter, e.g. 'en', 'de' (default: all languages)"),
  engines: tool.schema
    .string()
    .optional()
    .describe("Comma-separated engine list, e.g. 'google,duckduckgo' (default: all enabled engines)"),
  time_range: tool.schema
    .enum(["day", "week", "month", "year"])
    .optional()
    .describe("Restrict results to a time range"),
  category: tool.schema
    .string()
    .optional()
    .describe("Search category, e.g. 'general', 'it', 'science' (default 'general')"),
})

type SearchArgs = ReturnType<typeof searchArgs.parse>

const searchResponse = tool.schema.object({
  results: tool.schema.array(
    tool.schema.object({
      url: tool.schema.string(),
      title: tool.schema.string().nullish(),
      content: tool.schema.string().nullish(),
      engine: tool.schema.string().nullish(),
      engines: tool.schema.array(tool.schema.string()).optional(),
      publishedDate: tool.schema.string().nullish(),
    }),
  ),
  suggestions: tool.schema.array(tool.schema.string()).optional(),
  unresponsive_engines: tool.schema
    .array(tool.schema.tuple([tool.schema.string(), tool.schema.string()]))
    .optional(),
})

export default (async () => {
  return {
    tool: {
      search: tool({
        description:
          "Search the web with a local SearXNG metasearch engine. " +
          "Returns titles, URLs, snippets, and engine failure warnings. " +
          "Use for current information, documentation, packages, error messages, etc. " +
          "Use webfetch on a result URL when you need the full page.",
        args: searchArgs.shape,
        execute: async (args: SearchArgs, ctx: ToolContext) => {
          const page = args.page ?? 1
          const max = args.max_results ?? 6
          const url = new URL("/search", SEARXNG)
          url.searchParams.set("q", args.query)
          url.searchParams.set("format", "json")
          url.searchParams.set("pageno", String(page))
          url.searchParams.set("categories", args.category ?? "general")
          url.searchParams.set("language", args.language ?? "all")
          if (args.engines) url.searchParams.set("engines", args.engines)
          if (args.time_range) url.searchParams.set("time_range", args.time_range)

          const timeout = AbortSignal.timeout(TIMEOUT_MS)
          const signal = AbortSignal.any([ctx.abort, timeout])
          let data: ReturnType<typeof searchResponse.parse>
          try {
            const res = await fetch(url, { signal, headers: { Accept: "application/json" } })
            if (!res.ok) {
              const body = (await res.text()).replace(/\s+/g, " ").trim().slice(0, 500)
              return `SearXNG returned HTTP ${res.status}${body ? `: ${body}` : ""}`
            }

            const parsed = searchResponse.safeParse(await res.json())
            if (!parsed.success) return "SearXNG returned an unexpected response: expected a JSON results array."
            data = parsed.data
          } catch (e) {
            if (ctx.abort.aborted) throw ctx.abort.reason
            if (timeout.aborted) return `SearXNG request timed out after ${TIMEOUT_MS / 1000} seconds.`
            if (e instanceof SyntaxError) {
              return "SearXNG returned invalid JSON. Check that its JSON output format is enabled."
            }
            return `SearXNG request failed: ${e instanceof Error ? e.message : String(e)}`
          }

          const results = data.results.slice(0, max)
          const lines = results.map((r, i) => {
            const date = r.publishedDate ? ` (${r.publishedDate.slice(0, 10)})` : ""
            const snippet = r.content?.replace(/\s+/g, " ").trim()
            const engines = r.engines?.length ? r.engines : r.engine ? [r.engine] : []
            const engine = engines.length ? ` [via ${engines.join(", ")}]` : ""
            const title = r.title?.replace(/\s+/g, " ").trim() || "(no title)"
            return `${i + 1}. ${title}${date}${engine}\n   ${r.url}${snippet ? `\n   ${snippet}` : ""}`
          })
          const warnings = data.unresponsive_engines ?? []
          const sections = [
            data.results.length
              ? `Showing ${results.length} of ${data.results.length} results on page ${page} for "${args.query}":`
              : `No results on page ${page} for "${args.query}".`,
            ...lines,
          ]
          if (data.suggestions?.length) sections.push(`Suggestions: ${data.suggestions.join("; ")}`)
          if (warnings.length) {
            sections.push(
              `Some engines failed; results may be incomplete: ${warnings.map(([engine, reason]) => `${engine} (${reason})`).join("; ")}`,
            )
          }

          return {
            title: `search: ${args.query}`,
            output: sections.join("\n\n"),
            metadata: { query: args.query, page, count: data.results.length, shown: results.length, warnings },
          }
        },
      }),
    },
  }
}) satisfies Plugin
