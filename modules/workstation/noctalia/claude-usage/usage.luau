local usageCommand = "@USAGE_COMMAND@"
local panelId = "thunder/claude-usage:usage-panel"
local colors = { ok = "#80C080", warn = "#E0C070", over = "#E07070", error = "#E07070" }
local updating = false

local function barTree(usage)
    local tooltip = "updated " .. usage.updated
    if usage.stale then
        tooltip = tooltip .. " (stale, last fetch failed)"
    end

    local items = {
        ui.glyph({
            name = "gauge",
            size = 16,
            color = usage.stale and "on_surface/0.4" or colors[usage.severity],
        }),
    }
    for _, w in ipairs(usage.windows) do
        table.insert(items, ui.label({ text = w.short, fontSize = 11, color = "on_surface/0.6" }))
        table.insert(items, ui.progress({
            progress = w.used / 100,
            marker = w.elapsed / 100,
            fill = colors[w.severity],
            track = "surface_container_high",
            markerColor = "on_surface",
            width = 36,
            height = 6,
            radius = 3,
        }))
        table.insert(items, ui.label({
            text = tostring(math.floor(w.used + 0.5)) .. "%",
            fontSize = 12,
            fontWeight = "semibold",
        }))
    end
    return ui.row({ align = "center", gap = 6, tooltip = tooltip }, items)
end

local function showError(msg)
    barWidget.render(ui.row({
        align = "center",
        gap = 6,
        tooltip = msg,
    }, {
        ui.glyph({ name = "alert-circle", size = 16, color = colors.error }),
        ui.label({ text = "Claude unavailable", fontSize = 12 }),
    }))
end

local function refresh(force)
    if updating then
        return
    end

    updating = true
    local command = force and (usageCommand .. " --force") or usageCommand
    noctalia.runAsync(command, function(result)
        updating = false
        if result.exitCode ~= 0 then
            showError(result.stderr)
            return
        end

        local usage, err = noctalia.json.decode(result.stdout)
        if not usage then
            showError(err)
            return
        end

        noctalia.state.set("usage", usage)
        barWidget.render(barTree(usage))
    end)
end

function onClick()
    noctalia.togglePanel(panelId)
end

function onRightClick()
    refresh(true)
end

function update()
    noctalia.setUpdateInterval(60000)
    refresh(false)
end
