#!/usr/bin/env python3
import json
import sys

import humanize
import requests

domain = "http://torrent.home"
user = "admin"
passwd = "adminadmin"


def session():
    client = requests.Session()
    response = client.post(
        f"{domain}/api/v2/auth/login",
        data={"username": user, "password": passwd},
        timeout=5,
    )
    response.raise_for_status()
    return client


if len(sys.argv) > 1 and sys.argv[1] == "toggle_limit":
    session().post(f"{domain}/api/v2/transfer/toggleSpeedLimitsMode", timeout=5).raise_for_status()
elif len(sys.argv) == 1 or sys.argv[1] == "status":
    client = session()
    speed = client.get(f"{domain}/api/v2/transfer/info", timeout=5).json()
    mode = client.get(f"{domain}/api/v2/transfer/speedLimitsMode", timeout=5).json()
    print(json.dumps({
        "text": f"󰇚 {humanize.naturalsize(speed['dl_info_speed'], binary=True)}/s 󰕒 {humanize.naturalsize(speed['up_info_speed'], binary=True)}/s",
        "mode": "normal" if mode == 0 else "alternative",
    }))
else:
    raise SystemExit(f"unknown command: {sys.argv[1]}")
