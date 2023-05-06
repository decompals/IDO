//////////////////////////////////////////////////////////////
// PanelTest.c++
//////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include "PanelWindow.h"

// Main driver. Just instantiate a VkApp and the PanelWindow,
// "show" the window and then "run" the application.

void main ( int argc, char **argv )
{
    VkApp        *panelApp = new VkApp("panelApp", &argc, argv);
    PanelWindow  *panelWin = new PanelWindow("panelWin");

    panelWin->show();
    panelApp->run();
}
