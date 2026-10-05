local qbitCommand = "@QBIT_COMMAND@"
local updating = false

local function refresh()
    if updating then
        return
    end

    updating = true
    noctalia.runAsync(qbitCommand .. " status", function(result)
        updating = false
        if result.exitCode ~= 0 then
            barWidget.setGlyph("alert-circle")
            barWidget.setGlyphColor("error")
            barWidget.setText("qBittorrent unavailable")
            barWidget.setTooltip(result.stderr)
            return
        end

        local status, err = noctalia.json.decode(result.stdout)
        if not status then
            barWidget.setText("qBittorrent unavailable")
            barWidget.setTooltip(err)
            return
        end

        barWidget.setGlyph("download")
        barWidget.setGlyphColor(status.mode == "normal" and "#80C080" or "#E07070", "script")
        barWidget.setText(status.text)
        barWidget.setTooltip("Click to toggle alternative speed limits\nRight-click to open qBittorrent")
    end)
end

function update()
    noctalia.setUpdateInterval(2000)
    refresh()
end

function onClick()
    noctalia.runAsync(qbitCommand .. " toggle_limit", function()
        refresh()
    end)
end

function onRightClick()
    noctalia.runAsync("xdg-open http://torrent.home")
end
