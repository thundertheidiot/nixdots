local awful = require("awful")
local gears = require("gears")

local modkey = require("config/keys/modkey")

return gears.table.join(
	awful.key({ modkey, "Shift"}, "space", function(c)
		awful.client.floating.toggle()
		c.ontop = false
	end),
	awful.key({ modkey, }, "q", function(c)
		c:kill()
	end),
	awful.key( {modkey, "Shift"}, "comma", function(c)
		c:move_to_screen(c.screen:get_next_in_direction('left'))
	end),
	awful.key( {modkey, "Shift"}, "period", function(c)
		c:move_to_screen(c.screen:get_next_in_direction('right'))
	end),
	awful.key( {modkey, "Shift"}, "Return", function(c)
		c:swap(awful.client.getmaster())
	end)

)

