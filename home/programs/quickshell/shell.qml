import Quickshell
import QtQuick
import Niri 0.1

import "Components/Bar"

ShellRoot {
    id: root

    property color colBase: "#1e1e2e"

    Niri {
	id: niri
	Component.onCompleted: connect()

	onConnected: console.log("Connected to niri")
    }

    LazyLoader {
	active: true
	component: Bar {}
    }
}

