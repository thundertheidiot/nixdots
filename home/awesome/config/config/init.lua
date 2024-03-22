local gears = require("gears")
local awful = require("awful")

root.keys(require("config.keys.global"))
awful.rules.rules = require("config.rules")
awful.layout.layouts = require("config.layout")



require("config.screen")
require("config.signals")
require("config.notifications")

local helper = gears.filesystem.get_configuration_dir() .. "/config/swallow/helper"
	
awful.spawn.easy_async("gcc " .. helper .. ".c" .. " -O2 -o " .. helper, function()
	require("config.swallow")
end)

