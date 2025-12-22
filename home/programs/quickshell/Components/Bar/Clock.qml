import Quickshell
import QtQuick

Text {
    SystemClock {
	id: clock
	precision: SystemClock.Seconds
    }

    text: Qt.formatDateTime(clock.date, "yyyy-MM-dd hh:mm:ss")
    anchors.centerIn: parent
}
