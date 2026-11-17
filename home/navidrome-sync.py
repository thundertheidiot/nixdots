import hashlib
import json
import secrets
import sys
from datetime import datetime
from pathlib import Path
from urllib.parse import urlencode
from urllib.request import Request, urlopen

cfg = json.loads(Path(sys.argv[1]).read_text())
dry = "--dry-run" in sys.argv[2:]
password = Path(cfg["passwordFile"]).read_text().strip()
playlist_dir = Path(cfg["playlistDir"])
state_file = Path(cfg["stateFile"])
# navidrome library path -> mpd music dir subpath
libraries = {k.rstrip("/") + "/": v.rstrip("/") + "/" for k, v in cfg["libraries"].items()}


def api(endpoint, params=()):
    salt = secrets.token_hex(8)
    token = hashlib.md5((password + salt).encode()).hexdigest()
    auth = [("u", cfg["username"]), ("t", token), ("s", salt), ("v", "1.16.1"), ("c", "mpd-sync"), ("f", "json")]
    # post, so long playlists don't blow up the url
    req = Request(f"{cfg['url']}/rest/{endpoint}", data=urlencode(auth + list(params)).encode())
    with urlopen(req, timeout=60) as r:
        resp = json.load(r)["subsonic-response"]
    if resp["status"] != "ok":
        sys.exit(f"{endpoint}: {resp['error']['message']}")
    return resp


def real(path):
    if not path.startswith("/"):
        sys.exit("navidrome isn't reporting real paths, enable 'report real path' for the mpd-sync player")
    return path


def to_mpd(path):
    for server, local in libraries.items():
        if path.startswith(server):
            return local + path[len(server):]
    return None


def to_server(path):
    for server, local in libraries.items():
        if path.startswith(local):
            return server + path[len(local):]
    return None


_index = None


def song_index():
    global _index
    if _index is None:
        _index, offset = {}, 0
        while True:
            songs = api("search3", [("query", ""), ("artistCount", 0), ("albumCount", 0),
                                    ("songCount", 500), ("songOffset", offset)])["searchResult3"].get("song", [])
            _index.update({real(s["path"]): s["id"] for s in songs})
            if len(songs) < 500:
                break
            offset += 500
    return _index


def remote_tracks(pl):
    entries = api("getPlaylist", [("id", pl["id"])])["playlist"].get("entry", [])
    tracks = []
    for e in entries:
        p = to_mpd(real(e["path"]))
        if p is None:
            print(f"  skipping {e['path']}, not in any mapped library")
        else:
            tracks.append(p)
    return tracks


def push(name, tracks, pl):
    index = song_index()
    ids = []
    for t in tracks:
        sid = index.get(to_server(t) or "")
        if sid is None:
            print(f"  skipping {t}, not found in navidrome")
        else:
            ids.append(sid)
    print(f"{name}: mpd -> navidrome ({len(ids)} tracks)")
    if dry:
        return None
    target = [("playlistId", pl["id"])] if pl else [("name", name)]
    return api("createPlaylist", target + [("songId", i) for i in ids])["playlist"]["changed"]


def pull(name, tracks):
    print(f"{name}: navidrome -> mpd ({len(tracks)} tracks)")
    if dry:
        return None
    f = playlist_dir / f"{name}.m3u"
    f.write_text("".join(t + "\n" for t in tracks))
    return f.stat().st_mtime_ns


def read_local(f):
    lines = f.read_text().splitlines()
    return {"mtime": f.stat().st_mtime_ns, "tracks": [li for li in lines if li and not li.startswith("#")]}


state = json.loads(state_file.read_text()) if state_file.exists() else {}
local = {f.stem: read_local(f) for f in playlist_dir.glob("*.m3u")}
remote = {
    p["name"]: p
    for p in api("getPlaylists")["playlists"].get("playlist", [])
    if p.get("owner") == cfg["username"] and not p.get("readonly")
}

new_state = {}
for name in sorted(local.keys() | remote.keys()):
    loc, rem, prev = local.get(name), remote.get(name), state.get(name)

    if loc and rem:
        loc_changed = prev is None or prev["mpd"] != loc["mtime"]
        rem_changed = prev is None or prev["nd"] != rem["changed"]
        mtime, changed = loc["mtime"], rem["changed"]
        if loc_changed or rem_changed:
            tracks = remote_tracks(rem)
            if tracks != loc["tracks"]:
                rem_time = datetime.fromisoformat(changed).timestamp()
                if loc_changed and (not rem_changed or loc["mtime"] / 1e9 > rem_time):
                    changed = push(name, loc["tracks"], rem)
                else:
                    mtime = pull(name, tracks)
        new_state[name] = {"mpd": mtime, "nd": changed}

    elif loc and prev:
        print(f"{name}: deleted in navidrome, removing from mpd")
        if not dry:
            (playlist_dir / f"{name}.m3u").unlink()

    elif rem and prev:
        print(f"{name}: deleted in mpd, removing from navidrome")
        if not dry:
            api("deletePlaylist", [("id", rem["id"])])

    elif loc:
        new_state[name] = {"mpd": loc["mtime"], "nd": push(name, loc["tracks"], None)}

    else:
        new_state[name] = {"mpd": pull(name, remote_tracks(rem)), "nd": rem["changed"]}

if not dry:
    state_file.parent.mkdir(parents=True, exist_ok=True)
    state_file.write_text(json.dumps(new_state, indent=2))
