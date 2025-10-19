#!/usr/bin/env python3
import requests
import humanize
import math
import json
import time
import sys

domain = "http://torrent.home"
user = "admin"
passwd = "adminadmin"

session = requests.Session()
response = session.post(f"{domain}/api/v2/auth/login",
                     data={'username': user, 'password': passwd})

if len(sys.argv) > 1 and sys.argv[1] == "toggle_limit":
    session.post(f"{domain}/api/v2/transfer/toggleSpeedLimitsMode")
    exit(0)

def create_text():
    speed = session.get(f"{domain}/api/v2/transfer/info").json()
    mode = session.get(f"{domain}/api/v2/transfer/speedLimitsMode").json()

    dl: int = humanize.naturalsize(speed['dl_info_speed'], binary=True)
    up: int = humanize.naturalsize(speed['up_info_speed'], binary=True)

    text: str = f"󰇚 {dl}/s 󰕒 {up}/s"

    alt = "normal" if mode == 0 else "alternative"

    data = {
        'text': text,
        'alt': alt,
    }

    return json.dumps(data)

while True:
    print(create_text())
    sys.stdout.flush()
    time.sleep(2)
