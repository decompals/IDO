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

/////////////////////////////////////////////////////////////////////
// wmquit.c++. A simple extension of hello.c++ that handles
// the window manger quit message by asking "Are you sure..."
// before exiting.
////////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkQuestionDialog.h>
#include <Xm/Label.h>

// Define a top-level window class

class HelloWindow: public VkSimpleWindow {

  protected:

    Boolean HelloWindow::okToQuit();

  public:

    HelloWindow ( const char *name );
    ~HelloWindow();
    virtual const char* className();  // Identify this class
};

HelloWindow::HelloWindow ( const char *name ) : VkSimpleWindow ( name ) 
{
    Widget label =  XmCreateLabel ( mainWindowWidget(), "hello", NULL, 0 );

    addView(label);
}

const char * HelloWindow::className()
{
    return "HelloWindow";
}      

HelloWindow::~HelloWindow()
{
    // Empty
}

// Post a dialog to find out if the user wants to allow this window to
// be closed.

Boolean HelloWindow::okToQuit()
{
    return (theQuestionDialog->postAndWait("Do you really want to exit?"));
}

// Main driver. Just instantiate a VkApp and a top-level window, "show" 
// the window and then "run" the application.

void main ( int argc, char **argv )
{
    VkApp        *app = new VkApp("Hello", &argc, argv);
    HelloWindow  *win = new HelloWindow("hello");

    win->show();
    app->run();
}




