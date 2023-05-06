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
// hello.c++: This example displays a simple label in a window. The program
// instantiates a VkApp object and defines a toplevel window class,
// HelloWindow, which is a subclass of VkSimpleWindow.
// Unlike the "generic" examples, this is the more usual way
// to organize a program, as it provides a place to keep
// per-window state and a way to support per-window operations
////////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Xm/Label.h>

// Define a top-level window class

class HelloWindow: public VkSimpleWindow {

  protected:

    Widget _label;    // Hang on to widget a a data member

  public:

    HelloWindow ( const char *name );
    ~HelloWindow();
    virtual const char* className();  // Identify this class
};


// The HelloWindow constructor provides a place in which to create a
// widget tree to be installed as a "view" in the window.

HelloWindow::HelloWindow ( const char *name ) : VkSimpleWindow ( name ) 
{
    _label =  XmCreateLabel ( mainWindowWidget(), "hello", NULL, 0 );

    addView(_label);
}

const char * HelloWindow::className()
{
    return "HelloWindow";
}      

HelloWindow::~HelloWindow()
{
    // Empty
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




