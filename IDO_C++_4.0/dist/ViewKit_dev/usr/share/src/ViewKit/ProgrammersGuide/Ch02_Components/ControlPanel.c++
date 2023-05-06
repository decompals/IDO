//////////////////////////////////////////////////////////////
// ControlPanel.c++
//////////////////////////////////////////////////////////////

#include "ControlPanel.h"
#include <iostream.h>

ControlPanel::ControlPanel (const char *name , Widget parent) :
                                             StartStopPanel (name, parent)
{
    XtVaSetValues(_baseWidget, XmNorientation, XmVERTICAL, NULL);
}

ControlPanel::~ControlPanel()
{
    // Empty
}


const char* ControlPanel::className()
{
    return "ControlPanel";
}

void ControlPanel::start(Widget w, XtPointer callData)
{
    cout << "Process started\n" << flush;
    StartStopPanel::start(w, callData);
}

void ControlPanel::stop(Widget w, XtPointer callData)
{
    cout << "Process stopped\n" << flush;
    StartStopPanel::stop(w, callData);
}
