local gears = require("gears")
local awful = require("awful")

local function xrdb_command()

	local output = io.popen("themer --get-pywal-status")
	local result = output:read("*a")
	output:close()

	if result == "true" then
		return "xrdb -load " .. os.getenv("XDG_CACHE_HOME") .. "/wal/xresources"
	else
		return "xrdb -load " .. os.getenv("XRESOURCES")
	end
	
end

local system_autostart = {
	-- xfce nonsense
	"xfsettingsd",
	-- "/usr/lib/xfce4/notifyd/xfce4-notifyd",
	"/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1",

	"dbus-update-activation-environment --verbose --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY",
	"xset r rate 300 50",
	xrdb_command(),
	"themer --defaults-check",
	"setxkbmap -option caps:escape",
	-- "unclutter",
	"gajim",
	"emacs --daemon",
	"mpd",
	"nm-applet",
	"easyeffects",
	"ckb-next -b",
	"mailtimer",
	"xcompmgr",
}

for app = 1, #system_autostart do
	awful.spawn(system_autostart[app], false)
end

awful.spawn.easy_async_with_shell("sleep 0.3", function()
	awful.spawn.easy_async_with_shell("gnome-keyring-daemon --start --daemonize --components ssh", false)
end)


