pragma Singleton
import Quickshell
import Quickshell.Services.Pipewire
import QtQuick
import QtQuick.Layouts

Singleton {
    id: root

    readonly property PwNode defaultSink: Pipewire.defaultAudioSink
    readonly property PwNode defaultSource: Pipewire.defaultAudioSource

    function mutedOr(muted: bool, or: var) : string {
	if (muted) {
	    return "muted";
	} else {
	    return or;
	}
    }

    PwObjectTracker {
	objects: [ defaultSink, defaultSource ]
    }
}
