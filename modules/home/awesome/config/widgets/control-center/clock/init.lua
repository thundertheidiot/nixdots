local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")

local buttons = require("modules.awesome-buttons.awesome-buttons")

local clock = {}

local function worker(args)

	local popup = require("helpers.widget_popup") {
		hide_on_click_anywhere = false,
		hide_on_left_click = false,
		content_function = function() require("widgets.bar.volumeslider").get() end,
	}
	popup.placement = awful.placement.top_left

	local widget = require("widgets.control-center.components.clockbutton").create({
		type = basic,
		refresh = 1,
		format = "%d %b %Y (%a) %H:%M:%S",
		onclick = function() popup.visible = true end,

	})

    return widget
end

return setmetatable(clock, { __call = function(_, ...)
    return worker(...)
end })
