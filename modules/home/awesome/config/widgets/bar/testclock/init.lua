local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")

local clock = {}
local format = "%H:%M"

widget = require("modules.awesome-buttons.awesome-buttons").with_text({ text = "amogus" })
textbox = widget.widget.widget

--wibox.widget.textclock("%H:%M", 1)

function clock.update()
	textbox.text = os.date(format)
end

function clock.setup(table)
	format = table.format
	clock.update()
end

function clock.get()
	return widget
end

timer = gears.timer {
	timeout = 1
}

timer:connect_signal("timeout", function() clock.update() end)
timer:start()


return clock

