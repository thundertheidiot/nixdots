#!/usr/bin/env python3
import requests
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

    dl: int = math.ceil(speed['dl_info_speed'] / 1024)
    up: int = math.ceil(speed['up_info_speed'] / 1024)

    text: str = f"󰇚 {dl} kB/s 󰕒 {up} kB/s"

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
