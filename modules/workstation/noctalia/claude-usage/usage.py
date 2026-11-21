#!/usr/bin/env python3
import json
import os
import sys
import time
import urllib.error
import urllib.request
from datetime import datetime, timedelta, timezone

creds = os.path.expanduser("~/.config/claude/.credentials.json")
url = os.environ.get("CLAUDE_USAGE_URL", "https://api.anthropic.com/api/oauth/usage")
cache_path = os.environ.get("CLAUDE_USAGE_CACHE", os.path.expanduser("~/.cache/claude-usage/usage.json"))
force = "--force" in sys.argv[1:]
poll_interval = 900
default_backoff = 900
windows = [
    ("5h", "5-hour", "five_hour", timedelta(hours=5)),
    ("7d", "Weekly", "seven_day", timedelta(days=7)),
]
rank = {"ok": 0, "warn": 1, "over": 2}


def fail(msg):
    print(msg, file=sys.stderr)
    raise SystemExit(1)


def time_left(td):
    secs = int(td.total_seconds())
    if secs <= 0:
        return "now"
    days, secs = divmod(secs, 86400)
    hours, secs = divmod(secs, 3600)
    mins = secs // 60
    if days:
        return f"{days}d {hours}h"
    if hours:
        return f"{hours}h {mins:02d}m"
    return f"{mins}m"


def load_cache():
    try:
        with open(cache_path) as f:
            return json.load(f)
    except (OSError, json.JSONDecodeError):
        return {}


def save_cache(cache):
    os.makedirs(os.path.dirname(cache_path), exist_ok=True)
    tmp = cache_path + ".tmp"
    with open(tmp, "w") as f:
        json.dump(cache, f)
    os.replace(tmp, cache_path)


def build(data, fetched_at, stale):
    now = datetime.now(timezone.utc)
    out = []
    severity = "ok"
    for short, label, key, length in windows:
        w = data.get(key) or {}
        if w.get("utilization") is None or not w.get("resets_at"):
            continue

        used = float(w["utilization"])
        resets = datetime.fromisoformat(w["resets_at"])
        # window rolled over since the last fetch
        if resets <= now:
            used, resets = 0.0, resets + length
        elapsed = max(0.0, min(100.0, (now - (resets - length)) / length * 100))

        # more than 15 points ahead of the clock is over pace
        diff = used - elapsed
        level = "over" if diff > 15 else "warn" if diff > 0 else "ok"
        if rank[level] > rank[severity]:
            severity = level

        out.append({
            "short": short,
            "label": label,
            "used": round(used, 1),
            "elapsed": round(elapsed, 1),
            "resets_in": time_left(resets - now),
            "resets_at": resets.astimezone().strftime("%a %H:%M"),
            "severity": level,
        })

    age = now.timestamp() - fetched_at
    return {
        "windows": out,
        "severity": severity,
        "updated": "just now" if age < 60 else time_left(timedelta(seconds=age)) + " ago",
        "stale": stale,
    }


def emit(result):
    print(json.dumps(result))
    raise SystemExit(0)


cache = load_cache()
raw = cache.get("data")
fetched_at = cache.get("fetched_at", 0)


def back_off(seconds, msg):
    cache["blocked_until"] = time.time() + seconds
    save_cache(cache)
    if raw:
        emit(build(raw, fetched_at, True))
    fail(msg)


now_ts = time.time()
if raw and not force and now_ts < cache.get("next_fetch", 0):
    emit(build(raw, fetched_at, False))
if raw and now_ts < cache.get("blocked_until", 0):
    emit(build(raw, fetched_at, True))

try:
    with open(creds) as f:
        oauth = json.load(f)["claudeAiOauth"]
except (OSError, KeyError, json.JSONDecodeError) as e:
    back_off(60, f"can't read token from {creds}: {e}")

if oauth.get("expiresAt", 0) / 1000 < now_ts:
    back_off(60, "token expired, run claude once to refresh it")

req = urllib.request.Request(url, headers={
    "Authorization": f"Bearer {oauth['accessToken']}",
    "anthropic-beta": "oauth-2025-04-20",
    "User-Agent": "usage-widget/1.0",
})
try:
    with urllib.request.urlopen(req, timeout=15) as r:
        data = json.load(r)
except urllib.error.HTTPError as e:
    if e.code == 429:
        retry = e.headers.get("Retry-After", "")
        wait = min(int(retry), 3600) if retry.isdigit() else default_backoff
        back_off(wait, f"rate limited, retrying in {wait // 60}m")
    back_off(60, f"HTTP {e.code}")
except urllib.error.URLError as e:
    back_off(60, f"request failed: {e.reason}")

result = build(data, now_ts, False)
if not result["windows"]:
    back_off(60, "no usage data in response")

save_cache({
    "next_fetch": now_ts + poll_interval,
    "blocked_until": 0,
    "fetched_at": now_ts,
    "data": data,
})
emit(result)
