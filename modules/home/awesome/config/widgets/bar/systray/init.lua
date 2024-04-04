local awful = require("awful")
local wibox = require("wibox")

local systray = {}

-- local widget = wibox.widget.systray()

local widget = wibox.widget {
	{
		widget = wibox.widget.systray()
	},
	margins = { top = 5, bottom = 5 },
	widget = wibox.container.margin
}

function systray.get()
	return widget
end

return systray
