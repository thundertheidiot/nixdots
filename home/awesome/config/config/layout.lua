local gears = require("gears")
local awful = require("awful")

local l = awful.layout.suit

return {
	l.tile,
	l.max,
}
