local M = {}

function M.smart_screen_direction(direction)
	local screen = awful.screen.focus

	naughty.notify({ text = screen.name })

	if screen:get_next_in_direction(direction) ~= screen then
		return screen:get_next_in_direction(direction)
	else
		local reverse_dir
		if direction == "left" then reverse_dir = "right" elseif direction == "right" then reverse_dir = "left" end
		for _ in 1,awful.screen.count() do
			screen = screen:get_next_in_direction(reverse_dir)
		end
		return screen
	end
end

return M
