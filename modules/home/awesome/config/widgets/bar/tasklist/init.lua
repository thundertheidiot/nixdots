local awful = require("awful")
local wibox = require("wibox")

local tasklist = {}

local function worker(args)

	if not args.screen then
		return
	end

	tasklist.widget = awful.widget.tasklist {
		screen = args.screen,
		filter = awful.widget.tasklist.filter.currenttags,
		buttons = require("widgets.bar.tasklist.keys"),
		widget_template = {
     	{
        	{
             	{
                 	{
                     	id     = "icon_role",
                     	widget = wibox.widget.imagebox,
                 	},
					margins = 4,
                 	widget  = wibox.container.margin,
             	},
             	{
                	id     = "text_role",
                	widget = wibox.widget.textbox,
             	},
             layout = wibox.layout.fixed.horizontal,
         	},
        	left  = 4,
        	right = 4,
        	widget = wibox.container.margin
     	},
     	id     = "background_role",
     	widget = wibox.container.background,
 		}

	}

	return tasklist.widget
end

return setmetatable(tasklist, { __call = function(_, ...)
    return worker(...)
end })
