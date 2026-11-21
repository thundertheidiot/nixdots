local colors = { ok = "#80C080", warn = "#E0C070", over = "#E07070" }
local data = nil
local open = false

local function section(w)
    local diff = w.used - w.elapsed
    local pts = math.floor(math.abs(diff) + 0.5)
    local pace = pts == 0 and "on pace"
        or tostring(pts) .. " pts " .. (diff > 0 and "ahead of pace" or "under pace")

    return ui.column({ gap = 8 }, {
        ui.row({ align = "center", gap = 8 }, {
            ui.label({ text = w.label, fontSize = 13, fontWeight = "semibold" }),
            ui.spacer({ flexGrow = 1 }),
            ui.label({
                text = tostring(math.floor(w.used + 0.5)) .. "%",
                fontSize = 13,
                fontWeight = "bold",
                color = colors[w.severity],
            }),
        }),
        ui.progress({
            progress = w.used / 100,
            marker = w.elapsed / 100,
            fill = colors[w.severity],
            track = "surface_container_high",
            markerColor = "on_surface",
            height = 10,
            radius = 5,
        }),
        ui.row({ align = "center", gap = 8 }, {
            ui.label({ text = pace, fontSize = 11, color = "on_surface/0.6" }),
            ui.spacer({ flexGrow = 1 }),
            ui.label({
                text = "resets " .. w.resets_in .. " · " .. w.resets_at,
                fontSize = 11,
                color = "on_surface/0.6",
            }),
        }),
    })
end

local function render()
    if not data then
        panel.render(ui.column({ padding = 18, fill = "surface_container", radius = 18 }, {
            ui.label({ text = "Loading usage…", fontSize = 13, color = "on_surface/0.6" }),
        }))
        return
    end

    local children = {
        ui.row({ align = "center", gap = 8 }, {
            ui.glyph({ name = "gauge", size = 18, color = colors[data.severity] }),
            ui.label({ text = "Claude usage", fontSize = 15, fontWeight = "semibold" }),
            ui.spacer({ flexGrow = 1 }),
            ui.label({
                text = "updated " .. data.updated .. (data.stale and " · stale" or ""),
                fontSize = 11,
                color = "on_surface/0.6",
            }),
        }),
    }
    for _, w in ipairs(data.windows) do
        table.insert(children, section(w))
    end

    panel.render(ui.column({
        padding = 18,
        gap = 18,
        fill = "surface_container",
        radius = 18,
    }, children))
end

noctalia.state.watch("usage", function(value)
    data = value
    if open then
        render()
    end
end)

function onOpen()
    open = true
    data = noctalia.state.get("usage") or data
    render()
end

function onClose()
    open = false
end
