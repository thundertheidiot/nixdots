local gears = require("gears")
local awful = require("awful")

local opts = require("opts")

local modkey = require("config.keys.modkey")
local terminal = os.getenv("TERMINAL") or "st"
local browser = os.getenv("BROWSER") or "firefox"

local layouts = require("config.layout")
local volume = require("widgets.bar.volumeslider")

local helpers = require("helpers")

local function interm(command)
	return terminal .. " -e " .. command
end

local function center_mouse(c)
	local geom = c:geometry()
	mouse.coords({
		x = geom.x + geom.width / 2,
		y = geom.y + geom.height / 2,
	}, true) -- silent
end

return gears.table.join(
	require("config.keys.tagkeys"),
	-- Basics
	awful.key({ modkey, }, "j", function()
		-- awful.client.focus.byidx(1)

		local c = awful.client.next(1)
		client.focus = c
		c:raise()
		if awful.layout.get(awful.screen.focused()) ~= layouts[2] then
			center_mouse(c)
		end
	end),
	awful.key({ modkey, }, "k", function()
		-- awful.client.focus.byidx(-1)
		
		local c = awful.client.next(-1)
		client.focus = c
		c:raise()
		if awful.layout.get(awful.screen.focused()) ~= layouts[2] then
			center_mouse(c)
		end
	end),
	awful.key({ modkey, }, "h", function()
		awful.tag.incmwfact(-0.05)
	end),
	awful.key({ modkey, }, "l", function()
		awful.tag.incmwfact(0.05)
	end),
	awful.key({ modkey, }, "t", function()
		awful.layout.set(layouts[1], awful.screen.focused().selected_tag)
	end),
	-- awful.key({ modkey, }, "b", function()
	-- 	awful.screen.focused().bar.visible = not awful.screen.focused().bar.visible
	-- end),
	awful.key({ modkey, }, "f", function()
		if awful.layout.get(awful.screen.focused()) ~= layouts[2] then
			awful.layout.set(layouts[2], awful.screen.focused().selected_tag)
			if client.focus then
				client.focus:raise()
			end
		else
			awful.layout.set(layouts[1], awful.screen.focused().selected_tag)
		end
	end),

	awful.key({ modkey, "Shift"}, "f", function()
		if client.focus then
			local c = client.focus
			c.fullscreen = not c.fullscreen
			if c.fullscreen then
				c.border_width = 0
			else
				c.border_width = 2
			end
			c:raise()
		end
	end),

	awful.key({ modkey, }, "comma", function()
		client.focus = nil -- unfocus the current client, mouse will focus the new client on the other screen
		awful.screen.focus_bydirection("left")
	end),
	
	awful.key({ modkey, }, "period", function()
		client.focus = nil -- unfocus the current client, mouse will focus the new client on the other screen
		awful.screen.focus_bydirection("right")
	end),

	awful.key({ modkey, "Shift" }, "q", awesome.quit),
	awful.key({ modkey, "Shift" }, "r", awesome.restart),

	awful.key({ modkey, }, "v", function() opts.mouse_locked_to_window = not opts.mouse_locked_to_window end),


	-- Launching programs
	awful.key({ modkey, }, "Return", function()
		awful.spawn(terminal)
	end),
	awful.key({ modkey, }, "w", function()
		awful.spawn(browser)
	end),
	awful.key({ modkey, }, "y", function()
		awful.spawn("qm youtube")
	end),
	awful.key({ modkey, "Shift" }, "y", function()
		awful.spawn("qm")
	end),
	awful.key({ modkey, }, "m", function()
		awful.spawn(interm("ncmpcpp"))
	end),
	awful.key({ modkey, "Shift" }, "m", function()
		awful.spawn(interm("pulsemixer"))
	end),
	awful.key({ modkey, }, "e", function()
		awful.spawn("emacsclient -c")
	end),
	awful.key({ modkey, }, "c", function()
		awful.spawn("crtmenu")
	end),
	awful.key({}, "Print", function()
		awful.spawn("flameshot gui")
	end),
	awful.key({ modkey, }, "space", function()
		awful.spawn("kbdlayout")
	end),
	awful.key({ modkey, }, "d", function()
		awful.spawn("dmenu_run")
	end),
	awful.key({ modkey, }, "b", function()
		awful.spawn(interm("btop"))
	end),
	awful.key({ modkey, "Shift" }, "b", function()
		awful.spawn(interm("nvtop"))
	end),


	-- Media keys
	awful.key({}, "XF86AudioRaiseVolume", function()
		volume.increase(5)
	end),
	awful.key({}, "XF86AudioLowerVolume", function()
		volume.decrease(5)
	end),
	awful.key({}, "XF86AudioPlay", function()
		awful.spawn("mpc toggle")
	end),
	awful.key({}, "XF86AudioNext", function()
		awful.spawn("mpc next")
	end),
	awful.key({}, "XF86AudioPrev", function()
		awful.spawn("mpc prev")
	end),
	awful.key({ modkey, }, "p", function()
		awful.spawn("mpc toggle")
	end),
	awful.key({ modkey, "Shift" }, "p", function()
		awful.spawn("mpc next")
	end),
	awful.key({ modkey, }, "bracketleft", function()
		awful.spawn("mpc prev")
	end),
	awful.key({ modkey, }, "bracketright", function()
		awful.spawn("mpc next")
	end),
	awful.key({ modkey, "Shift" }, "bracketleft", function()
		awful.spawn("mpc seek -10")
	end),
	awful.key({ modkey, "Shift" }, "bracketright", function()
		awful.spawn("mpc seek +10")
	end)

)
