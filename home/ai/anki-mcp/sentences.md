---
name: sentences
description: Quiz the user with example sentences built from the Anki cards they studied in a given time window (today, yesterday, this week, last N days, a specific deck). Uses the read-only anki MCP server. Use when the user asks for example sentences, practice sentences, a quiz, or to be tested on what they learned or reviewed in Anki.
---

# sentences

Build practice sentences from words the user actually studied in Anki, then
quiz them. Anki desktop with AnkiConnect must be running, otherwise the anki
MCP tools fail, tell the user to open Anki.

## 1. Pick the time window

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
add `deck:"Kaishi 1.5k"` when they name that deck. Run `list_decks` if unsure.

## 2. Fetch the words

1. `search_notes` with the query. It returns at most 100 ids plus a `count`,
   if count > 100 say so and work from the first 100
2. `get_notes_info` on the ids
3. Pull the target word from the fields. For the `Kaishi 1.5k` model use
   `Word`, `Word Reading`, `Word Meaning`, ignore audio/picture/pitch HTML.
   For other models look for the obvious term/meaning fields
4. Strip HTML (`<b>`, `<br>`, `[sound:...]`) before using values

If nothing matches, say so and suggest a wider window.

## 3. Write the sentences

- Every sentence must use at least one studied word, try to combine 2-3 so
  sentences are natural and cover more words
- Keep the rest of the vocabulary and grammar at or below the user's level,
  infer it from the deck (Kaishi 1.5k = beginner, around N5/N4). Don't lean on
  words they haven't seen when a known one works
- Do NOT reuse the card's own `Sentence` field, write fresh ones
- Japanese: write in natural Japanese, give furigana for kanji the user may
  not know as `漢字[かんじ]` only when asked or when a kanji is outside the
  studied set
- Default to about 10 sentences or one per word, whichever is smaller,
  unless the user asks for a number

## 4. Quiz modes

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
again in Anki. The MCP is read-only, never claim to have changed cards.
