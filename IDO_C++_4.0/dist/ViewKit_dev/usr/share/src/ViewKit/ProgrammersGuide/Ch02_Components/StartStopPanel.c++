/////////////////////////////////////////////////////////////
// StartStopPanel.c++
/////////////////////////////////////////////////////////////

#include "StartStopPanel.h"
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

// These are default resources for widgets in objects of this class
// All resources will be prepended by *<name> at instantiation,
// where <name> is the name of the specific instance, as well as the
// name of the baseWidget. These are only defaults, and may be
// overriden in a resource file by providing a more specific resource
// name

String StartStopPanel::_defaultResources[] = {
  "*start.labelString:  Start",
  "*stop.labelString:   Stop",
  "*orientation:	HORIZONTAL",
  NULL
};

const char *const StartStopPanel::actionCallback = "actionCallback";

StartStopPanel::StartStopPanel(const char *name, Widget parent) : 
                                                      VkComponent(name)
{
  // Load class-default resources for this object before creating base widget

  setDefaultResources(parent, _defaultResources );

  // Create an XmRowColumn widget as the component's base widget
  // to contain the buttons. Assign the widget to the _baseWidget
  // data member.
 
  _baseWidget = XmCreateRowColumn ( parent, _name, NULL, 0 );

  // Set up callback to handle widget destruction

  installDestroyHandler();

  // Create all other widgets as children of the base widget.
  // Manage all child widgets.

  _startButton = XmCreatePushButton ( _baseWidget, "start", NULL, 0);
  _stopButton  = XmCreatePushButton ( _baseWidget, "stop", NULL, 0);

  XtManageChild(_startButton);
  XtManageChild(_stopButton);

  // Install static member functions as callbacks for the buttons

  XtAddCallback(_startButton, XmNactivateCallback,
                &StartStopPanel::startCallback, (XtPointer) this );

  XtAddCallback(_stopButton, XmNactivateCallback,
                &StartStopPanel::stopCallback, (XtPointer) this );
}

StartStopPanel::~StartStopPanel()
{
  // Empty
}

const char* StartStopPanel::className()
{
    return "StartStopPanel";
}

void StartStopPanel::startCallback(Widget w, XtPointer clientData,
                                   XtPointer callData)
{
  StartStopPanel *obj = ( StartStopPanel * ) clientData;
  obj->start(w, callData);
}

void StartStopPanel::stopCallback(Widget w, XtPointer clientData,
                                  XtPointer callData)
{
  StartStopPanel *obj = ( StartStopPanel * ) clientData;
  obj->stop(w, callData);
}

void StartStopPanel::start(Widget, XtPointer)
{
  callCallbacks(actionCallback, (void *) START);
}

void StartStopPanel::stop(Widget, XtPointer)
{
  callCallbacks(actionCallback, (void *) STOP);
}
