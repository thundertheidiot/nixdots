local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local barpos = "top"

bar = {}

local function delimeter(string)
	return wibox.widget { text = string, widget = wibox.widget.textbox }
end

function bar.make(table)

	if table.screen == nil then
		return false
	end

	bar = awful.wibar({ position = barpos, screen = table.screen, bg = _G.beautiful.bg_normal })

	local battery = require("widgets.bar.battery")

	local volume = require("widgets.bar.volumeslider")
	volume.update_slider()

	local clock = require("widgets.control-center.clock")({ format = "%d %b %Y (%a) %H:%M:%S" })
	--clock.setup( { format = "%d %b %Y (%a) %H:%M:%S", } )

	
	local systray = require("widgets.bar.systray")

	bar:setup {
		layout = wibox.layout.align.horizontal,
		{
			layout = wibox.layout.fixed.horizontal,
			require("widgets.bar.taglist").make(table.screen),
		},
		require("widgets.bar.tasklist") { screen = table.screen },
		{
			layout = wibox.layout.fixed.horizontal,
			systray.get(),
			delimeter(" "),
			battery.get(),
			delimeter(" "),
			volume.get(),
			delimeter(" | "),
			clock,
		},
	}

	return bar
end

return bar
