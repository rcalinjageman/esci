import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "esci"
	title		: qsTr("esci")
	description	: qsTr("This module offers analyses.")
	version		: "0.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	icon		: "esci_logo.svg"


	Analysis
	{
		title:	qsTr("A test analysis")
		menu:	qsTr("First test")
		qml:	"esci_test.qml"
		func:	"esci_hello"
	}

	Analysis
	{
		title:	qsTr("Describe")
		menu:	qsTr("Describe")
		qml:	"esci_describe.qml"
		func:	"esci_describe"
	}


}
