////////////////////////////////////////////////////////////////////////////////
///////   Copyright 1992, Silicon Graphics, Inc.  All Rights Reserved.   ///////
//                                                                            //
// This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;     //
// the contents of this file may not be disclosed to third parties, copied    //
// or duplicated in any form, in whole or in part, without the prior written  //
// permission of Silicon Graphics, Inc.                                       //
//                                                                            //
// RESTRICTED RIGHTS LEGEND:                                                  //
// Use,duplication or disclosure by the Government is subject to restrictions //
// as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data     //
// and Computer Software clause at DFARS 252.227-7013, and/or in similar or   //
// successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -    //
// rights reserved under the Copyright Laws of the United States.             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

#ifndef VKSIMPLEWINDOW_H
#define VKSIMPLEWINDOW_H

// VkSimpleWindow.h - declarations for a Motif toplevel window object

class VkApp;

#include <Vk/VkComponent.h>


class VkSimpleWindow : public VkComponent {

    friend VkApp;

  public:

    VkSimpleWindow(const char *name, ArgList argList = NULL, Cardinal argCount = NULL);    
    virtual ~VkSimpleWindow();

    void addView(Widget);
    void addView(VkComponent *);

    void removeView();

    virtual void open();
    virtual void raise();
    virtual void lower();
    virtual void iconize();  // obsolete
    virtual void iconify();
    virtual void show();
    virtual void hide();

    virtual void build();   // obsolete, don't use directly

    const char *getTitle();
    void setTitle(const char *);
    void setIconName(const char *);
    void setClassHint(const char *className);

    virtual const char* className();
    Boolean iconic() const { return (_iconState == CLOSED); }
    Boolean visible() const { return (_visibleState == VISIBLE); }
    virtual Widget     mainWindowWidget() const;
    virtual Widget     viewWidget() const;
    virtual operator Widget () const;

  protected:

    enum IconState     { OPEN, CLOSED, ICON_UNKNOWN        };
    enum VisibleState  { HIDDEN, VISIBLE, VISIBLE_UNKNOWN  };
    enum StackingState { RAISED, LOWERED, STACKING_UNKNOWN };

    IconState      _iconState;
    VisibleState   _visibleState;
    StackingState  _stackingState;
    Widget         _mainWindowWidget;

    Boolean        _usingSetUpInterface; 
    virtual Widget setUpInterface(Widget );

    virtual void setUpWindowProperties();
    virtual void stateChanged(IconState);
    virtual void handleWmDeleteMessage();
    virtual void handleWmQuitMessage();    
    virtual void handleRawEvent(XEvent *event);
    virtual void afterRealizeHook();

    virtual void busy();
    virtual void veryBusy();
    virtual void notBusy();

  private:

    static  void stateChangedEventHandler(Widget, XtPointer, XEvent *, Boolean *);
    static  void deleteWindowCallback(Widget, XtPointer, XtPointer);
    static  void quitAppCallback(Widget, XtPointer, XtPointer);    

    Widget     _viewWidget;
    Window     _busyWindow;
    Boolean    _busy;

    
};


extern void VkConfigureWM(VkSimpleWindow *w,
			  Boolean quit    = TRUE,
			  Boolean close   = TRUE,
			  Boolean border  = TRUE,
			  Boolean title   = TRUE,
			  Boolean resize  = TRUE,
			  Boolean iconify = TRUE,
			  Boolean menu    = TRUE);

#endif









