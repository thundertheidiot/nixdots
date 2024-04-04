local awful = require("awful")
local wibox = require("wibox")

local clock = {}
local format = "%H:%M"

local widget = wibox.widget.textclock("%H:%M", 1)

function clock.setup(table)
	widget.format = table.format
end

function clock.get()
	return widget
end

return clock
