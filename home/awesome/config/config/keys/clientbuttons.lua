local gears = require("gears")
local awful = require("awful")

local modkey = require("config/keys/modkey")

return gears.table.join(
	awful.button({}, 1, function(c)
		c:emit_signal("request::activate", "mouse_click", {raise = true})

		if mouse.current_client ~= nil then
			client.focus = mouse.current_client
		end
	end),
    awful.button({ modkey }, 1, function (c)
		if c.maximized then
			c.maximized = false
		end
		local gv = c:geometry()
		if c.floating ~= true then
			c.floating = true
			c.ontop = true
			c:geometry(gv)
		end
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
		if c.maximized then
			c.maximized = false
		end
		local gv = c:geometry()
		if c.floating ~= true then
			c.floating = true
			c.ontop = true
			c:geometry(gv)
		end
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)
