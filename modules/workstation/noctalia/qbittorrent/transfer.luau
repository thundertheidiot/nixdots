local curl = "@CURL@"
local xdgOpen = "@XDG_OPEN@"
local url = "@URL@"
local api = url .. "/api/v2/"
local modeColors = { normal = "#80C080", alternative = "#E07070" }
local updating = false

local function get(path, callback)
    noctalia.runAsync(curl .. " -sf " .. api .. path, callback)
end

local function speed(bytes)
    local units = { "B", "KiB", "MiB", "GiB" }
    local i = 1
    while bytes >= 1024 and i < #units do
        bytes = bytes / 1024
        i = i + 1
    end
    return string.format(i == 1 and "%d %s/s" or "%.1f %s/s", bytes, units[i])
end

local function showError(msg)
    updating = false
    barWidget.render(ui.row({ align = "center", gap = 6, tooltip = msg }, {
        ui.glyph({ name = "alert-circle", size = 16, color = "error" }),
        ui.label({ text = "qBittorrent unavailable", fontSize = 12 }),
    }))
end

local function render(info, mode)
    local alt = mode == "1"
    barWidget.render(ui.row({
        align = "center",
        gap = 4,
        tooltip = (alt and "Alternative" or "Normal")
            .. " speed limits\nClick to toggle\nRight-click to open qBittorrent",
    }, {
        ui.glyph({ name = "download", size = 16, color = modeColors[alt and "alternative" or "normal"] }),
        ui.label({ text = speed(info.dl_info_speed), fontSize = 12 }),
        ui.glyph({ name = "upload", size = 16, color = "on_surface/0.6" }),
        ui.label({ text = speed(info.up_info_speed), fontSize = 12 }),
    }))
end

local function refresh()
    if updating then
        return
    end

    updating = true
    get("transfer/info", function(result)
        if result.exitCode ~= 0 then
            showError(result.stderr ~= "" and result.stderr or "curl exited " .. result.exitCode)
            return
        end

        local info, err = noctalia.json.decode(result.stdout)
        if not info then
            showError(err)
            return
        end

        get("transfer/speedLimitsMode", function(modeResult)
            updating = false
            render(info, string.match(modeResult.stdout, "%d"))
        end)
    end)
end

function update()
    noctalia.setUpdateInterval(2000)
    refresh()
end

function onClick()
    noctalia.runAsync(curl .. " -sf -X POST " .. api .. "transfer/toggleSpeedLimitsMode", function()
        refresh()
    end)
end

function onRightClick()
    noctalia.runAsync(xdgOpen .. " " .. url)
end
