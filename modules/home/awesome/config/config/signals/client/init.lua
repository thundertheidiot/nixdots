local awful = require("awful")
-- local opts = require("opts")

client.connect_signal("request::manage", function (c)
    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end

	c.size_hints_honor = false
	c.is_fixed = false

end)

client.connect_signal("mouse::enter", function(c) client.focus = c end)
	
-- client.connect_signal("mouse::leave", function(c)
-- 	if opts.mouse_locked_to_window then
-- 		local cgeom = c:geometry()
-- 		local mcoords = mouse.coords()
--
-- 		local newx = mcoords.x <= cgeom.x and cgeom.x or mcoords.x >= (cgeom.x + cgeom.width) and cgeom.x + cgeom.width or mcoords.x
-- 		local newy = mcoords.y <= cgeom.y and cgeom.y or mcoords.y >= (cgeom.y + cgeom.height) and cgeom.y + cgeom.height or mcoords.y
--
-- 		mouse.coords({x = newx, y = newy}, true)
-- 	end
-- end)

client.connect_signal("focus", function(c) c.border_color = _G.beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = _G.beautiful.border_normal end)

-- This is the "fuck off client" section

client.disconnect_signal("request::activate", awful.ewmh.activate)
client.connect_signal("request::activate", function(c)
	if c.context ~= "ewmh" then
		awful.ewmh.activate(c)
	end
end)

client.connect_signal("property::minimized", function(c) c.minimized = false end)
client.connect_signal("property::maximized", function(c)
	c.maximized = false
	c.maximize_horizontal = false
	c.maximize_vertical = false
end)

client.disconnect_signal("request::geometry", awful.ewmh.client_geometry_requests)
