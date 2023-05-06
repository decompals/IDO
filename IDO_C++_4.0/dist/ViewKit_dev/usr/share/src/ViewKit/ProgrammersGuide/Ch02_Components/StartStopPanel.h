//////////////////////////////////////////////////////////////
// StartStopPanel.h
//////////////////////////////////////////////////////////////

#ifndef _STARTSTOPPANEL_H
#define _STARTSTOPPANEL_H
#include <Vk/VkComponent.h>

enum PanelAction { START, STOP };

class StartStopPanel : public VkComponent {

  public:
    StartStopPanel (const char *, Widget);
    ~StartStopPanel();
    virtual const char *className();

    static const char *const actionCallback;

  protected:
    virtual void start(Widget, XtPointer);
    virtual void stop(Widget, XtPointer);

    Widget _startButton;
    Widget _stopButton;

  private:
    static void startCallback(Widget, XtPointer, XtPointer);
    static void stopCallback(Widget, XtPointer, XtPointer);

    static String _defaultResources[];
};

#endif
