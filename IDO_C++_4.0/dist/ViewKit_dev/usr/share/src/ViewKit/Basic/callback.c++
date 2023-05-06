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
//////////////////////////////////////////////////////////////////////////////
// Simple example that demonstrates how to use Xt callbacks in the ViewKit
//////////////////////////////////////////////////////////////////////////////
#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Xm/PushB.h>
#include <iostream.h>

class CallbackWindow: public VkSimpleWindow {

  private:

    // Declare a callback as a static member function

    static void pushCallback(Widget, XtPointer, XtPointer);

  protected:

    // Declare a  regular member function to be called by a static callback

    virtual void push(Widget, XtPointer);

  public:

    CallbackWindow ( const char *name );
    ~CallbackWindow ( );
    virtual const char* className();
};

CallbackWindow::CallbackWindow ( const char *name ) : VkSimpleWindow ( name ) 
{
    // Create a pushbutton

    Widget pb =  XmCreatePushButton ( mainWindowWidget(), "Callback", NULL, 0 );

    // Install the callback, passing the this pointer as clientData

    XtAddCallback(pb, XmNactivateCallback, 
		  &CallbackWindow::pushCallback,
		  (XtPointer) this);


    addView(pb);
}

CallbackWindow::~CallbackWindow ( )
{
  // Empty
}

const char* CallbackWindow::className() { return "CallbackWindow"; }


void CallbackWindow::pushCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    // Extract the object from clientData

    CallbackWindow *obj = (CallbackWindow *) clientData;

    // Call the corresponding function

    obj->push(w, callData);
}

void CallbackWindow::push(Widget, XtPointer)
{
    // Do something

    cout << "Push!!\n" << flush;
}


void main ( int argc, char **argv )
{
    VkApp           *app = new VkApp("CallbackApp", &argc, argv);
    CallbackWindow  *win = new CallbackWindow("Callback");

    win->show();
    app->run();
}

