import xbmcaddon
import xbmcgui
import xbmc
import os
 
addon       = xbmcaddon.Addon()
addonname   = addon.getAddonInfo('name')

i = xbmcgui.Dialog().select("Launch:", ['Home', 'Yle Areena', 'Youtube TV'])

programs = ["firefox_tv", "areena_tv", "youtube_tv"]
os.system(programs[i])
# xbmc.executebuiltin("System.Exec({exec})".format(exec = programs[i]))
