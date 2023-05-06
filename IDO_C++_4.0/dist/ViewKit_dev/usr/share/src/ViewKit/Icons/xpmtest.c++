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

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkIcon.h>
#include <Xm/PushB.h>

/* XPM */
static char *readwrite[] = {
/* width height ncolors chars_per_pixel */
"39 15 3 1",
/* colors */
"` c #000000",
"a c #EEEEEE",
"b c #BBBBBB s background",
/* pixels */
"bbbbbbbbbbbbbbbbbbbbbb`bbbbbbbbbbbbbbbb",
"bbbbbbbbbbbbbbbbbbbbb`a`bbbbbbbbbbbbbbb",
"bbbbbbbbbbbbbb``bbbb`ab`bbbbbbb``bbbbbb",
"bbbbbbbbbbbbb`bb`bb`b``bbbbbbb`bb`bbbbb",
"bbbbbbbbbbbbb`bbb`ab``bbbbbbbb`bbb`bbbb",
"bbbbbbbbbbbbb`bbb``a`bbbbbbbbb`bbbb`bbb",
"bbbbbbbbbbbbbbbb`b``bbbbbbbbbbbbbbbb`bb",
"bbbbbbbbbbbbbbb`a``b`a`````bbbbba`````b",
"bbbbbbbbbbbbbb`a``bb```bbbb`bbbb`bbbb``",
"bbbbbbbbbbbbb`a``bbbb`baabbb````baabbb`",
"bbbbbbbbbbbb`a``bbbbb`baabbb`bb`baabbb`",
"bbbbbbbbbbb`a``bbbbbb`bbbbba`bb`bbbbba`",
"bbbbbbbbbb`a``bbbbbbb`bbbbab`bb`bbbbab`",
"bbbbbbbbbbb``bbbbbbbbb`bbab`bbbb`bbab`b",
"`b`````bb``bbbbbbbbbbbb````bbbbbb````bb"
};


char *colors[] = {
    "highlightColor1",
     NULL
};


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
    _label =  XmCreatePushButton ( mainWindowWidget(), "hello", NULL, 0 );

    XtManageChild(_label);
    
    XtVaSetValues(_label, XmNlabelType, XmPIXMAP,
		  XmNlabelPixmap, VkCreateXPMPixmap(_label, readwrite, colors),
		  NULL);

    addView( _label );
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




