local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

function set_wallpaper(s)
	awful.spawn("setbg")
end

awful.screen.connect_for_each_screen(function(s)
	set_wallpaper(s)

	awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

	s.bar = require("widgets.bar").make({ screen = s })
end)
