local awful = require("awful")


return {
	{
		rule = { },
		properties = {
			border_width = beautiful.border_width,
			border_color = beautiful.border_normal,
			focus = awful.client.focus.filter,
			raise = true,
			keys = require("config/keys/client"),
			buttons = require("config/keys/clientbuttons"),
        	screen = awful.screen.preferred,
        	placement = awful.placement.no_overlap+awful.placement.no_offscreen,
			size_hints_honor = false,
			is_fixed = false,
		},
	},
	{
		rule_any = {
			class = {
				"Godot",
			}
		},
		properties = {
			focus = false,
		}
	},
	{
		rule_any = {
			class = {
				"xfce4-notifyd"
			},
			type = {
				"dialog",
				"utility",
				"toolbar",
				"splash",
			},
		},
		properties = {
			floating = true,
			ontop = true,
			-- border_width = 0,
			-- border_color = 0,
		},
	},
	{
		rule = { class = "Steam" },
		properties = { tag = "9" },
	},
	{
		rule = { class = "cs2" },
		properties = { border_width = 0 },
	},
	{
		rule = { class = "steam" },
		properties = { tag = "9" },
	},
	{
		rule = { class = "Gajim" },
		properties = { tag = "7" },
	},
	{
		rule = { class = "easyeffects" },
		properties = { tag = "6" },
	},

}
