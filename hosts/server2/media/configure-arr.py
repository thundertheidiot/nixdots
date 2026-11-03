"""Reconcile exported settings absent from nixflix's public options.

Runs at service startup only. No collection deletion, media scanning, or searches.
"""
import argparse
import json
from pathlib import Path
from urllib.request import Request, urlopen


def resolve_secrets(value):
    if isinstance(value, dict):
        if set(value) == {"_secret"}:
            return Path(value["_secret"]).read_text().strip()
        return {key: resolve_secrets(item) for key, item in value.items()}
    if isinstance(value, list):
        return [resolve_secrets(item) for item in value]
    return value


def reconcile(request, settings):
    for endpoint, desired in settings.get("singletons", {}).items():
        current = request("GET", endpoint)
        request("PUT", endpoint, current | desired | {"id": current["id"]})
    for endpoint, desired_items in settings.get("collections", {}).items():
        def identity(item):
            if endpoint == "remotepathmapping":
                return item["host"], item["remotePath"]
            return item["name"]
        existing = {identity(item): item for item in request("GET", endpoint)}
        for desired in desired_items:
            current = existing.get(identity(desired))
            if current:
                request("PUT", f"{endpoint}/{current['id']}",
                        current | desired | {"id": current["id"]})
            else:
                request("POST", endpoint, desired)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("settings")
    parser.add_argument("app")
    parser.add_argument("base_url")
    parser.add_argument("api_key_file")
    args = parser.parse_args()
    api_key = Path(args.api_key_file).read_text().strip()

    def request(method, endpoint, body=None):
        req = Request(args.base_url + "/" + endpoint,
                      data=None if body is None else json.dumps(body).encode(),
                      headers={"X-Api-Key": api_key, "Content-Type": "application/json"},
                      method=method)
        with urlopen(req, timeout=60) as response:
            raw = response.read()
            return json.loads(raw) if raw else None

    reconcile(request, resolve_secrets(json.loads(Path(args.settings).read_text())[args.app]))


if __name__ == "__main__":
    main()
