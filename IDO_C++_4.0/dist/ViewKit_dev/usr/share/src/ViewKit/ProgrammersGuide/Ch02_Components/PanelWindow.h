//////////////////////////////////////////////////////////////
// PanelWindow.h
//////////////////////////////////////////////////////////////

#ifndef _PANELWINDOW_H
#define _PANELWINDOW_H

#include <Vk/VkSimpleWindow.h>
#include "StartStopPanel.h"

// Define a top-level window class

class PanelWindow: public VkSimpleWindow {

  public:
    PanelWindow(const char *name);
    ~PanelWindow();
    virtual const char* className();

  protected:
    void statusChanged(VkCallbackObject *, void *, void *);

    StartStopPanel * _controlPanel;

  private:
    static String _defaultResources[];
};

#endif
