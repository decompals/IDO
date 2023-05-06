//////////////////////////////////////////////////////////////
// PanelTest2.c++
//////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include "ControlPanel.h"

// Main driver. Instantiate a VkApp, a VkSimpleWindow, and a
// ControlPanel, add the ControlPanel as the SimpleWindow's view,
// "show" the window and then "run" the application.

void main ( int argc, char **argv )
{
    VkApp *panelApp = new VkApp("panel2App", &argc, argv);
    VkSimpleWindow *panelWin = new VkSimpleWindow("panelWin");
    ControlPanel *control = new ControlPanel("control",
                                             panelWin->mainWindowWidget() );

    panelWin->addView(control);
    panelWin->show();
    panelApp->run();
}
