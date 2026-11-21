---
name: anki
description: Work with the user's Anki collection through the read-only anki MCP server. Covers decks, card and note lookup, review history, retention, due and lapsed cards, forecasts, and study coaching. Has a sentences sub mode that builds practice sentences from recently studied cards and quizzes the user. Use for any Anki question, progress check, deck or card lookup, or when the user asks for example sentences, a quiz, or to be tested on what they studied.
---

# anki

Read the user's Anki collection through the `anki` MCP server and answer from
its data. Anki desktop with AnkiConnect must be running; if the calls fail,
tell the user to open Anki. The server is read-only: never claim to have
edited, suspended, or rated a card.

## Calling the server

Tool names below are the anki MCP tool names. Call them with whatever MCP
tooling the harness provides.

Albedo only: MCP is reached from Python. Call tools as
`await mcp.call("anki/<tool>", {...})`, check `r.isError`, run independent
calls with `await parallel(...)`, and use `await mcp.tools(server="anki")` /
`await mcp.describe("<name>")` for schemas.

## What it can do

Group the request into one of these areas, then pick the tools.

| area | tools | use it for |
| --- | --- | --- |
| decks | `list_decks`, `get_deck_stats`, `get_deck_config`, `get_deck_progress` | deck names, due/new/learning counts, mature vs young vs unseen, average ease and interval, new-per-day and review limits |
| find cards and notes | `search_cards`, `search_notes` (Anki query syntax, e.g. `deck:"X"`, `tag:y`, `is:due`, `rated:N`, `introduced:N`, `prop:ivl>21`) | locating cards or notes, counting matches; `search_notes` returns ids and a `count` |
| card and note details | `get_cards_info`, `get_notes_info`, `get_model_fields`, `list_models`, `list_tags` | fields, tags, note type, deck, reps, lapses, interval, ease for given ids; field names of a note type |
| card state | `check_cards_due`, `check_cards_suspended`, `get_card_intervals`, `get_card_ease_factors` | whether specific cards are due or suspended, their interval (negative = seconds, positive = days) and ease (2500 = 250%) |
| history | `get_review_history`, `get_cards_reviewed_today`, `get_card_reviews`, `get_reviews_of_cards`, `get_time_stats` | daily review counts, reviews of a deck or of given cards over N days, time spent and hour-of-day patterns |
| retention | `calculate_retention`, `get_retention_trend` | retention rate for a deck over N days, optionally split mature vs young, daily or weekly trend |
| problem cards | `find_lapsed_cards`, `find_low_ease_cards` | cards you keep forgetting (most lapses) or find hard (low ease), optionally per deck, with limit |
| workload | `get_forecast`, `get_study_summary`, `get_collection_stats_html` | upcoming reviews per day, a coaching summary (totals, due today, streak, 7-day average, recent activity), full stats report as HTML |

Typical flows:

- "how am I doing": `get_study_summary`, then `calculate_retention` for the
  deck they care about
- "what should I focus on": `find_lapsed_cards` and `find_low_ease_cards`
  for that deck, then `get_cards_info` on the ids for their fields
- "what's coming": `get_forecast` with a deck and `days`
- "what did I do": `get_cards_reviewed_today` or `get_review_history`
- "what's in deck X": `list_decks` to confirm the exact name, then
  `get_deck_progress` or `get_deck_stats`

Deck names are exact and case-sensitive, and subdecks use `::`. Call
`list_decks` if unsure. Results are JSON text; parse them before summarising.
Keep numbers faithful to the tool output, and say when a tool result was
capped (for example `count` above the ids returned).

## Sub mode: sentences

Use this when the user asks for example sentences, practice sentences, a
quiz, or to be tested on what they studied. Build practice sentences from
words they actually studied, then quiz them.

### 1. Pick the time window

Translate the request into an Anki search. Days roll over at Anki's day
boundary, `rated:1` means today.

| request          | query                         |
| ---------------- | ----------------------------- |
| today            | `rated:1`                     |
| yesterday        | `rated:2 -rated:1`            |
| today+yesterday  | `rated:2`                     |
| this week        | `rated:7`                     |
| last N days      | `rated:N` (max 365)           |
| N days ago only  | `rated:N -rated:N-1`          |
| new words only   | `introduced:N` instead        |
| words I failed   | add `rated:N:1` (pressed Again) |

Default to `rated:1` if no window is given. Scope to vocab, skip kana drills:
prefix with `-deck:"Japanese::1 Hiragana"` unless the user asks for kana, or
add `deck:"Kaishi 1.5k"` when they name that deck.

### 2. Fetch the words

1. `search_notes` with the query. It returns at most 100 ids plus a `count`;
   if count > 100 say so and work from the first 100
2. `get_notes_info` on the ids
3. Pull the target word from the fields. For the `Kaishi 1.5k` model use
   `Word`, `Word Reading`, `Word Meaning`, ignore audio/picture/pitch HTML.
   For other models use `get_model_fields` and pick the obvious term and
   meaning fields
4. Strip HTML (`<b>`, `<br>`, `[sound:...]`) before using values

If nothing matches, say so and suggest a wider window.

### 3. Write the sentences

- Every sentence must use at least one studied word, try to combine 2-3 so
  sentences are natural and cover more words
- Keep the rest of the vocabulary and grammar at or below the user's level,
  infer it from the deck (Kaishi 1.5k = beginner, around N5/N4). Don't lean on
  words they haven't seen when a known one works
- Do NOT reuse the card's own `Sentence` field, write fresh ones
- Japanese: write in natural Japanese, give furigana as `漢字[かんじ]` only
  when asked or when a kanji is outside the studied set
- Default to about 10 sentences or one per word, whichever is smaller,
  unless the user asks for a number

### 4. Quiz modes

Pick from the request, default to **translate to English**.

- **translate to English** (default): show the Japanese sentence, user
  translates, then check
- **translate to Japanese**: show English, user produces Japanese, accept any
  correct phrasing and point out unnatural bits
- **fill the gap**: Japanese sentence with the target word blanked, give the
  English meaning as a hint
- **just show me**: list sentences with readings and translations, no quiz

Ask one sentence at a time and wait for the answer. After each answer give a
short verdict, the correct version, and a note on the studied word if they
missed it. At the end, list the words they got wrong so they can look at them
again in Anki.

## Other requests

For anything outside the sub mode, pick the area from the table, make the
smallest set of calls that answers it, and answer directly with the numbers
and the ids or deck names they need. If a question needs data the server
does not expose, say so instead of guessing.
