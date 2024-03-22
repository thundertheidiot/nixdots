local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

battery = {}

local format = " {} "

local widget = wibox.widget{
	text = "",
	align = "center",
	valign = "center",
	widget = wibox.widget.textbox,
}

function battery.update()
	awful.spawn.easy_async("sb-battery", function(stdout)
		-- widget.text = format:gsub("{}", tostring(stdout))
		widget.text = stdout
	end)
end

function battery.setup(table)
	format = table.format
	battery.update()
end

function battery.get()
	return widget
end

local timer = gears.timer {
	timeout = 10,
	call_now = true,
	autostart = true,
	callback = function()
		battery.update()
	end,
}

timer:start()

return battery
