import xbmcaddon
import xbmcgui
import xbmc
import os
import subprocess
import json
 
addon       = xbmcaddon.Addon()
addonname   = addon.getAddonInfo('name')

home = os.path.expanduser("~")
json_path = os.path.join(home, 'kodi_launcher.json')

try:
    with open(json_path, 'r') as f:
        program_data = json.load(f)
except Exception as e:
    xbmcgui.Dialog().ok("Launcher error", f"Could not load {json_path}:\n{e}")
    raise SystemExit

names = list(program_data.keys())

index = xbmcgui.Dialog().select("Launch:", names)

if index != -1:
    selected = names[index]
    executable = program_data[selected]

    with open('/home/cage/kodi_launcher.log', 'w') as f:
        subprocess.Popen([executable], stdout=f, stderr=f)

# selected = names[index]
# executable = program_data[selected]
# subprocess.run([executable])
