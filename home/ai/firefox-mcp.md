---
name: firefox-mcp
description: Drive Firefox via the firefox-devtools / firefox-devtools-attach MCP servers. Use when opening, reading, clicking, filling or screenshotting pages in Firefox, or when those tools error.
---

# Firefox MCP

Two servers:
- `firefox-devtools`: launches its own window on the agent profile (`~/.local/share/firefox-devtools-mcp/firefox_devtools_mcp_profile`).
- `firefox-devtools-attach`: drives a window the user opened with `ai-firefox`. Fails until one exists. Never close their tabs; close what you opened.

Attach when a window is open, own otherwise. Firefox locks profiles: one process per profile.

## Bindings
- `mcp.firefox_devtools` / `mcp.firefox_devtools_attach` (underscores).
- Results are records: `r.content[0].text`. No `isError` — errors arrive as text starting with `Error:`.
- Save-yielding tools return the written path in that text.

## Loop
`list_pages` → `new_page`/`select_page` → `take_snapshot` → `click_by_uid`/`fill_by_uid`/`type_text` → `evaluate_script` → `close_page`.

- UIDs die on navigation or DOM change: re-snapshot.
- `fill_by_uid` for plain inputs; `type_text` for autocomplete/rich fields.
- `evaluate_script` for one value, `get_page_text` for prose.
- `screenshot_page(saveTo=true)`, parse the path, `show_image` it. saveTo only accepts cwd-relative paths or `~/.firefox-devtools-mcp/output/`.
- Downloads: `set_download_behavior(behavior="allowed")` first, then `list_downloads`.

## Gotchas
- Marionette pins `navigator.webdriver=true`; bot-walls may challenge regardless of cookies.
- `close_firefox_session` releases an attach connection (browser stays up), closes an own instance.
- After 30 idle minutes an attach session releases Firefox; the next tool call reconnects and selects the user's active tab.
