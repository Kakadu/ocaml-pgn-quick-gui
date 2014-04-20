import QtQuick 2.1
import QtQuick.Controls 1.0
import QtQuick.Layouts 1.0

ApplicationWindow {
    // next two properties regulate how big  text blocks and latters will be
    property int defaultFontSize: 19
    property int defaultTextFieldHeight: defaultFontSize + 4
    property string backgroundColor: "#FFFFDF"
    property int cellSize: 50

    Keys.onEscapePressed: Qt.quit()

    ExclusiveGroup {
        Action {
            id: api_browsing_action
            text: "Api Browsing"
            checkable: true
            Component.onCompleted: checked = true
            onTriggered: {
                root.applyPaths();
                root.state = "BROWSE_API";
            }
        }
        Action {
            id: path_editing_action
            text: "Path Editing"
            checkable: true
            Component.onCompleted: checked = false
            onTriggered: {
                root.setCurrentPaths();
                root.state = "EDIT_PATHS";
            }
        }
    }


    Rectangle {
        id: root
        color: backgroundColor
        anchors.fill: parent
        focus: true

        Component {
            id: cellDelegate
            Item {
                width: boardView.cellWidth;
                height: boardView.cellHeight
                Rectangle { anchors.fill:parent;
                            color: {
                                if(homm.isWhite()) return "white";
                                else return "darkgray";
                            }
                          }
                Image { anchors.fill:parent;
                        source: {
                            if (homm.picture == "") return "";
                            else {
                                //console.log("QML img: " + homm.picture);
                                return ("svgpieces/" + homm.picture);
                            }
                        }
                      }
            }
        }

        GridView {
            id: boardView
            cellWidth: cellSize
            cellHeight: cellSize
            width: cellSize * 8
            height: cellSize * 8
            model: boardModel
            delegate: cellDelegate
        }

        Rectangle {
          // buttons below board
          anchors {
            left: boardView.left
            right: boardView.right
            bottom: root.bottom
            top: boardView.bottom
          }
          MouseArea {
            anchors.fill: parent
            onClicked: controller.doE2E4()
          }
          Text {
            anchors.centerIn: parent
            text: "e2-e4"
          }
        }
        ScrollView {
          id: moves
          anchors { left: boardView.right
                    right: root.right
                    bottom: root.bottom
                    top: root.top
                  }
          Layout.minimumHeight: 100
          Layout.maximumHeight: 400
          height: 300

          TextEdit {
            id: movesField
            width: 800;
            font.family: "Monospace"
            font.pixelSize: defaultFontSize
            focus: true
            selectByMouse: true
            readOnly: true
            textFormat: TextEdit.RichText
            wrapMode: TextEdit.WordWrap

            onLinkActivated: controller.moveSelected(link);
            text: {
                if (controller.hasData) controller.movesText
                else ""
            }
          }
        }

    }
}
