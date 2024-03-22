local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")

require("config.autostart")

require("awful.autofocus")

_G.beautiful = require("beautiful")
_G.beautiful.init(gears.filesystem.get_configuration_dir() .. "/theme/theme.lua")
--_G.beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")

require("config")

-- local popup = require("widgets.popup") { 
-- 	widget = {
-- 		widget = wibox.container.margin,
-- 		margins = 10,
-- 		{
-- 			widget = wibox.widget.textbox,
-- 			text = "testpopup",
-- 		},
-- 	},
-- 	placement = awful.placement.top_left,
-- 	shape = gears.shape.rounded_rect,
-- 	visible = true,
-- 	hide_on_click = true,
-- 	hide_on_left_click = true,
-- }
