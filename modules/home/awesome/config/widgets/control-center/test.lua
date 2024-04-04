local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local popup = require("helpers.widget_popup") {
	hide_on_left_click = true,
	maximum_width = 400,
	content_widget = wibox.widget.textbox("amogus")
}

return popup
