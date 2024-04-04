local awful = require("awful")
local wibox = require("wibox")

volume = {}

local format = " {} "

local widget = wibox.widget{
	markup = " piibel tuieotasldkf;j ;lsdkaj;lksda",
	align = "center",
	valign = "center",
	widget = wibox.widget.textbox,
}

function volume.update()
	awful.spawn.easy_async("wpctl get-volume @DEFAULT_AUDIO_SINK@", function(stdout)
		local num = math.floor(stdout:gsub("Volume: ", "") * 100)
		widget.text = format:gsub("{}", num)
	end)
end

function volume.increase(amount)
	awful.spawn.easy_async("wpctl set-volume @DEFAULT_AUDIO_SINK@ " .. amount .. "%+", function()
		volume.update()
	end)
end

function volume.decrease(amount)
	awful.spawn.easy_async("wpctl set-volume @DEFAULT_AUDIO_SINK@ " .. amount .. "%-", function()
		volume.update()
	end)
end

function volume.setup(table)
	format = table.format
	volume.update()
end


function volume.get()
	return widget
end

return volume
