import Quickshell
import Quickshell.Widgets
import Quickshell.Services.SystemTray
import QtQuick
import QtQuick.Layouts
import "Clock"
import "Audio"

PanelWindow {
    required property var modelData
    screen: modelData

    anchors {
	top: true
	left: true
	right: true
    }

    implicitHeight: 30

    Row {
	id: leftRow

	anchors.verticalCenter: parent.verticalCenter
	anchors.left: parent.left

	height: parent.height
	    spacing: 7
	
	Text {
	    text: niri.focusedWindow?.title ?? ""
	anchors.verticalCenter: parent.verticalCenter
	}

	Text {
	    text: niri.focusedWindow?.title ?? ""
	anchors.verticalCenter: parent.verticalCenter
	}
    }

    Clock {}

    Row {
	id: rightRow

	anchors.verticalCenter: parent.verticalCenter
	anchors.right: parent.right

	height: parent.height

	Audio {}

	RowLayout {
	    anchors.centerIn: parent
	    spacing: 7

	    Repeater {
		model: SystemTray.items

		delegate: Rectangle {
		    required property var modelData
		    Layout.alignment: Qt.AlignCenter

		    IconImage {
			anchors.centerIn: parent
			width: 20
			height: 20
			source: modelData.icon
		    }
		}
	    }
	}
    }
}
