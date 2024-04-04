local awful = require("awful")

taglist = {}

function taglist.make(s)
	return awful.widget.taglist {
		screen = s,
		filter = awful.widget.taglist.filter.all,
		buttons = require("widgets.bar.taglist.keys")
	}
end

return taglist
