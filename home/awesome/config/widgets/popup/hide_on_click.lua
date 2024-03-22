local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local click_to_hide = require("helpers.click_to_hide")

local default_config = {
	ontop = true,
	visible = true,
	hide_on_click = false,
	hide_on_left_click = false,
	widget = {
		wibox.widget.textbox,
		text = "placeholder",
	}
}

local function create_popup(args)
	local config = gears.table.join(default_config, args)

	local popup = awful.popup(config)

	-- if config.hide_on_click then
	-- end
		
	click_to_hide.popup(popup, nil, true)

	local can_toggle = true
    local toggle_lock_timer = gears.timer {
        timeout = 0.1,
        single_shot = true,
        callback  = function()
            can_toggle = true
        end
    }
    popup:connect_signal("property::visible",
                         function()
                             can_toggle = false
                             toggle_lock_timer:again()
                         end
    )

	function popup:toggle(force)
		if can_toggle then
			if force == false or (force == nil and self.visible) then
				self.visible = false
			else
				self.visible = true
			end
		end
	end

	return popup
end

return create_popup
