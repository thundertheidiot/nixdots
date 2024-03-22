local awful = require("awful")
local gears = require("gears")
require("awful.autofocus")

local termclass = "St"
local noswallow = { }

local function is_in_table(table, element)
	for _, value in pairs(table) do
		if element:match(value) then
			return true
		end
	end
	return false
end

local function can_swallow(class)
	return not is_in_table(noswallow, class)
end

local function is_terminal(c)
	if c.class and c.class == termclass then
		return true
	else
		return false
	end
end

local function copy_size(c, parent)
	if not c or not parent then
		return
	end
	if not c.valid or not parent.valid then
		return
	end

	local geo = parent:geometry()
	c:geometry(geo)
end

local function turn_off(c, current_tag)
    if current_tag == nil then
        current_tag = c.screen.selected_tag
    end
    local ctags = {}
    for k, tag in pairs(c:tags()) do
        if tag ~= current_tag then
            table.insert(ctags, tag)
        end
    end
    c:tags(ctags)
    c.sticky = false
end

local function turn_on(c)
    local current_tag = c.screen.selected_tag
    ctags = { current_tag }
    for k, tag in pairs(c:tags()) do
        if tag ~= current_tag then
            table.insert(ctags, tag)
        end
    end
    c:tags(ctags)
    c:raise()
    client.focus = c
end

local function sync(to_c, from_c)
    if not from_c or not to_c then
        return
    end
    if not from_c.valid or not to_c.valid then
        return
    end
    if from_c.modal then
        return
    end
    to_c.floating = from_c.floating
    to_c.maximized = from_c.maximized
    to_c.above = from_c.above
    to_c.below = from_c.below
    to_c:geometry(from_c:geometry())
    -- TODO: Should also copy over the position in a tiling layout
end

-- local function get_parent_pid(child_ppid, callback)
--     local ppid_cmd = string.format("pstree -ps %s", child_ppid)
--     awful.spawn.easy_async(ppid_cmd, function(stdout, stderr, reason, exit_code)
--         -- primitive error checking
--         if stderr and stderr ~= "" then
--             callback(stderr)
--             return
--         end
--         local ppid = stdout
--         callback(nil, ppid)
--     end)
-- end

-- client.connect_signal("property::size", check_resize_client)
-- client.connect_signal("property::position", check_resize_client)

local function setup_swallow(client, parent)
	sync(client, parent)
	turn_off(parent)
	client:connect_signal("unmanage", function()
		if parent then
			turn_on(parent)
			sync(parent, client)
		end
	end)
end

client.connect_signal("manage", function(c)

	if is_terminal(c) then
		return
	end

	for _, p in ipairs(client.get()) do
		if not p then
			return
		end

		awful.spawn.easy_async(gears.filesystem.get_configuration_dir() .. "/config/swallow/helper " .. p.pid .. " " .. c.pid, function(output)
			if p == c then
				return
			end
			if tonumber(output) == p.pid and is_terminal(p) and can_swallow(c.class) then
				setup_swallow(c, p)
			end
		end)
	end


	-- get_parent_pid(c.pid, function(err, parent_pid)
	-- 	
	-- 	if err then
	-- 		error(err)
	-- 		return
	-- 	end
	--
	-- 	for _, p in ipairs(client.get()) do
	-- 		if (parent_pid:find("(" .. p.pid .. ")")) and can_swallow(c.class) and is_terminal(p) then
	-- 			setup_swallow(c, p)
	-- 			goto stop
	-- 		end
	-- 	end
	--
	-- 	::stop::
	--
	-- end)
	
end)

-- client.connect_signal("manage", function(c)
--     if is_terminal(c) then
--         return
--     end
--
-- 	local parent_client=awful.client.focus.history.get(c.screen, 1)
--
-- 	get_parent_pid(c.pid, function(err, parent_pid)
-- 		if err then
-- 			error(err)
-- 			return
-- 		end
-- 		if parent_client and (parent_pid:find("(" .. parent_client.pid .. ")")) and can_swallow(c.class) and is_terminal(parent_client) then
-- 			sync(c, parent_client)
-- 			turn_off(parent_client)
-- 			-- parent_client.child_resize = c
-- 			-- parent_client.minimized = true
--         	c:connect_signal("unmanage", function() 
-- 				if parent_client then
-- 					turn_on(parent_client)
-- 					sync(parent_client, c)
-- 				end
-- 			end)
--         	-- copy_size(c, parent_client)
-- 		end
-- 	end)
--
-- end)
