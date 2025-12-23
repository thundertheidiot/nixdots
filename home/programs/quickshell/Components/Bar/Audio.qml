import Quickshell
import QtQuick
import QtQuick.Layouts
import qs.Services

RowLayout {
    Text {
	text: Pipewire.defaultSource.nickname 
    }

    Text {
	text: Pipewire.defaultSink.audio.muted ? "muted" : Pipewire.defaultSink.nickname
    }
}
