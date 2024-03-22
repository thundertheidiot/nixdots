local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")

volume = {}

local buttons = gears.table.join(
	awful.button({}, 4, function()
		volume.increase(5)
	end),
	awful.button({}, 5, function()
		volume.decrease(5)
	end)
)

local widget = wibox.widget{
	{
		widget = wibox.widget.slider,
		max_value = 1,
		value = 0.5,
		forced_width = 70,
		bar_height = 3,
		handle_shape = gears.shape.circle,
		handle_color = _G.beautiful.bg_focus,
		shape = gears.shape.rounded_bar,
		margins = { top = 5, bottom = 5 },
		paddings = 1,
		buttons = buttons,
	},
	layout = wibox.container.place,
}

local slider = widget.widget

local tooltip = awful.tooltip {
	objects = { widget },
}

tooltip:add_to_object(widget)

function volume.update_slider()
	awful.spawn.easy_async("wpctl get-volume @DEFAULT_AUDIO_SINK@", function(stdout)
		local num = math.floor(stdout:gsub("Volume: ", "") * 100)
		slider.value = num
		tooltip.text = num .. "%"
	end)
end

function volume.increase(amount)
	awful.spawn.easy_async("wpctl set-volume @DEFAULT_AUDIO_SINK@ " .. amount .. "%+", function()
		volume.update_slider()
	end)
end

function volume.decrease(amount)
	awful.spawn.easy_async("wpctl set-volume @DEFAULT_AUDIO_SINK@ " .. amount .. "%-", function()
		volume.update_slider()
	end)
end

function volume.get()
	return widget
end

slider:connect_signal("property::value", function()
	awful.spawn.easy_async("wpctl set-volume @DEFAULT_AUDIO_SINK@ " .. slider.value .. "%")
	tooltip.text = slider.value .. "%"
end)

widget:connect_signal("mouse::enter", function()
	tooltip.text = slider.value .. "%"
end)

return volume
