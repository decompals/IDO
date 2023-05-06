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

///////////////////////////////////////////////////////////////////////////
// busy.c++ A simple example of an application that calls busy to post 
// a dialog that reports ongoing progress.
//////////////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkFormat.h>
#include <Xm/PushB.h>

// Declare a top-level window class

class BusyWindow: public VkSimpleWindow {

  protected:

    static void busyCallback(Widget, XtPointer, XtPointer);

  public:

    BusyWindow ( const char *name );
   ~BusyWindow();
    virtual const char* className();
};

// Constructor creates a pushbutton to launch an example task,
// and installs it as a view in the top-level window

BusyWindow::BusyWindow(const char *name) : VkSimpleWindow ( name )
{
    Widget button =  XmCreatePushButton ( mainWindowWidget(), "Busy", NULL, 0 );
    
    XtAddCallback(button, XmNactivateCallback, 
		  &BusyWindow::busyCallback, 
		  (XtPointer) this);

    addView(button);
}

BusyWindow::~BusyWindow()
{
    // Empty
}

const char* BusyWindow::className() 
{ 
    return "BusyWindow"; 
}

// Simulate a busy task. Just loop for a while, reporting 
// progress occasionally

void BusyWindow::busyCallback(Widget, XtPointer clientData, XtPointer)
{
    int i, j, k;

    // Notify user that we are about to be "busy"

    theApplication->busy("Chug, Chug, Chug...", (VkSimpleWindow*) clientData);

    for(i=0; i<10; i++)
    {
	// Report progress so far

	theApplication->progressing(VkFormat("Chug, Chug, Chug... %d%% Done", i * 10));
	
	for(j=0; j< 1000; j++)
	    for(k=0; k<1000; k++)
		;
    }

    // Done! Report not busy

    theApplication->notBusy();    
}

// Main driver

void main ( int argc, char **argv )
{
    VkApp       *app  = new VkApp("BusyApp", &argc, argv);
    BusyWindow  *win  = new BusyWindow("Busy");
    BusyWindow  *win2 = new BusyWindow("Busy2");

    win->show();
    win2->show();

    app->run();
}




