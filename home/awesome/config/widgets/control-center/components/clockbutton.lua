local wibox = require("wibox")
local gears = require("gears")

local buttons = {}

buttons.create = function(args)
    local type = args.type or 'basic'
	local refresh = args.refresh or 1
	local format = args.format or "%H:%M"
    local onclick = args.onclick or function() end
    local color = args.color or '#D8DEE9'
    local text_size = args.text_size or 10

    local result = wibox.widget{
        {
            {
				format = format,
				refresh = refresh,
                widget = wibox.widget.textclock
            },
            top = 4, bottom = 4, left = 8, right = 8,
            widget = wibox.container.margin
        },
        bg = '#00000000',
        shape = function(cr, width, height) gears.shape.rounded_rect(cr, width, height, 4) end,
        widget = wibox.container.background
    }

    if type == 'outline' then
        result:set_shape_border_color(color)
        result:set_shape_border_width(1)
    elseif type == 'flat' then
        result:set_bg(color)
    end

    local old_cursor, old_wibox
    result:connect_signal("mouse::enter", function(c)
        if type ~= 'flat' then
            c:set_bg("#00000066")
        end
        local wb = mouse.current_wibox
        old_cursor, old_wibox = wb.cursor, wb
        wb.cursor = "hand1"
    end)
    result:connect_signal("mouse::leave", function(c)
        if type ~= 'flat' then
            c:set_bg('#00000000')
        end
        if old_wibox then
            old_wibox.cursor = old_cursor
            old_wibox = nil
        end
    end)

    result:connect_signal("button::press", function() onclick() end)

    return result
end

return buttons
