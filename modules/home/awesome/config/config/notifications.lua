local naughty = require("naughty")

local function merge(one, two)
	for k,v in pairs(two) do
		one[k] = v
	end

	return one
end

naughty.connect_signal("request::display", function(n)
    naughty.layout.box {
		notification = merge(n, {
			timeout = 5,
		}),
		bg = beautiful.bg_normal,
		fg = beautiful.fg_normal,
		timeout = 5,
	}
	-- naughty.notification {
	-- 	notification = n,
	-- 	bg = beautiful.bg_normal,
	-- 	fg = beautiful.bg_normal,
	-- }
end)
