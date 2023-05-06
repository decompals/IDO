//////////////////////////////////////////////////////////////
// PanelWindow.c++
//////////////////////////////////////////////////////////////

#include "PanelWindow.h"
#include <iostream.h>

String PanelWindow::_defaultResources[] =
        { "*controlPanel*orientation:  VERTICAL",
          NULL
        };

PanelWindow::PanelWindow(const char *name) : VkSimpleWindow (name) 
{
    setDefaultResources( mainWindowWidget(), _defaultResources );
    _controlPanel = new StartStopPanel( "controlPanel",
                                        mainWindowWidget() );

    _controlPanel->addCallback( StartStopPanel::actionCallback, this,
                         (VkCallbackMethod) &PanelWindow::statusChanged );

    addView(_controlPanel);
}

const char * PanelWindow::className()
{
    return "PanelWindow";
}      

PanelWindow::~PanelWindow()
{
    // Empty
}

void PanelWindow::statusChanged(VkCallbackObject *obj,
                                void *, void *callData)
{
    StartStopPanel * panel = (StartStopPanel *) obj;
    PanelAction action = (PanelAction) callData;
    switch (action) {
        case START:
            cout << "Process started\n" << flush;
            break;
        case STOP:
            cout << "Process stopped\n" << flush;
            break;
        default:
            cout << "Undefined state\n" << flush;
    }
}
