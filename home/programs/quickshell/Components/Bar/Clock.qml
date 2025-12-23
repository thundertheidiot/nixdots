import Quickshell
import QtQuick

Text {
    SystemClock {
	id: clock
	precision: SystemClock.Minutes
    }

    text: Qt.formatDateTime(clock.date, "yyyy-MM-dd hh:mm")
    anchors.centerIn: parent
}
