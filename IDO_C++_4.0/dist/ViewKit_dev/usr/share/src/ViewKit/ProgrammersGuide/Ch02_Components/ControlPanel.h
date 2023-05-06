//////////////////////////////////////////////////////////////
// ControlPanel.h
//////////////////////////////////////////////////////////////

#ifndef _CONTROLPANEL_H
#define _CONTROLPANEL_H
#include "StartStopPanel.h"

class ControlPanel : public StartStopPanel {

  public:
    ControlPanel (const char *, Widget);
    ~ControlPanel();
    virtual const char *className();

  protected:
    virtual void start(Widget, XtPointer);
    virtual void stop(Widget, XtPointer);
};

#endif
