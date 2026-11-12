---
name: ghidra-mcp
description: Reverse-engineer binaries locally with headless Ghidra via ghidra-mcp. List functions, imports, exports, strings, segments; decompile to C; follow xrefs; read memory; rename symbols; set types, prototypes and comments. Use when asked to analyze, reverse, decompile, disassemble, or inspect a binary, executable, shared library, ELF, or object file, or to investigate what a program does.
---

# ghidra-mcp

`ghidra-mcp` (on PATH, packaged in nixdots) runs full Ghidra headless in one
process. **One binary per process**, chosen at startup; there is no tool to
switch programs. Each fresh process re-analyzes unless you pass `--project`.

## One-shot calls

```
ghmcp BINARY TOOL ['{"json": "args"}'] ['--' extra flags]
```

`ghmcp` spawns the server, does the MCP handshake, calls one tool, prints its
text, exits. First call on a binary costs ~30s (JVM boot + auto-analysis);
large binaries take minutes — use a generous timeout (600s is safe).

Start with `get_program_info` to confirm format/language, then narrow down.

## Tools

inspect: `get_program_info` `list_functions` `list_methods` `search_functions_by_name`
`list_classes` `list_namespaces` `list_segments` `list_imports` `list_exports`
`list_data_items` `list_strings` `get_function_by_address` `get_current_address`
`get_current_function` `decompile_function` `decompile_function_by_address`
`disassemble_function` `get_xrefs_to` `get_xrefs_from` `get_function_xrefs`
`get_function_calls` `read_memory`

modify (in-memory unless saved): `rename_function` `rename_function_by_address`
`rename_data` `rename_variable` `set_function_prototype` `set_local_variable_type`
`set_decompiler_comment` `set_disassembly_comment`

Check a tool's argument schema before calling it with non-empty args.

## Multi-step sessions

For iterative work on one binary, keep one process alive instead of paying
analysis per call: spawn `ghidra-mcp --headless --binary BIN --ready-timeout 600`
as a background job, then speak newline-delimited JSON-RPC on its stdio:
`initialize` (id 1) -> `notifications/initialized` -> `tools/call` (id 2, 3, ...).
Responses carry the matching id; logs go to stderr only.

## Useful server flags (after `--` in ghmcp)

- `--project DIR` — persist analysis and edits across runs (auto-saves)
- `--no-analyze` — skip auto-analysis for a quick look
- `--language x86:LE:64:default --compiler gcc` — for non-native formats
- `--ready-timeout N` — how long tool calls wait for analysis (default 30)

## Notes

- NixOS: there is no `/bin/*`; use `/run/current-system/sw/bin/...` or nix store paths
- renames/types/comments are lost in temp projects; pass `--project` to keep them
