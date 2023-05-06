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
// interrupt.c++ A simple example of an application that allows interrupts
//               while performing a lengthy task
//////////////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkInterruptDialog.h>
#include <Xm/PushB.h>


class BusyWindow: public VkSimpleWindow {

  protected:

    static void busyCallback(Widget, XtPointer, XtPointer);

  public:

    BusyWindow ( const char *name );
    ~BusyWindow ();
    virtual const char* className();
};


BusyWindow::BusyWindow ( const char *name ) : VkSimpleWindow ( name )
{
  Widget button =  XmCreatePushButton ( mainWindowWidget(), "Begin Task", NULL, 0 );

  XtAddCallback(button, XmNactivateCallback, 
		&BusyWindow::busyCallback, 
		(XtPointer) this);

  addView(button);
}

BusyWindow::~BusyWindow ( )
{
  // Empty
}

const char* BusyWindow::className() { return "BusyWindow"; }

void BusyWindow::busyCallback(Widget, XtPointer clientData, XtPointer)
{
    int i, j, k = 0;

    // Install the interruptible dialog as the dialog to post when busy

    theApplication->setBusyDialog(theInterruptDialog);

    // Start being "busy"

    theApplication->busy("Very Busy", (BusyWindow *) clientData);

    k = 0;

    for(i=0; i<10000; i++)
    {
	// Every so often, see if the task was interrupted

	if(theInterruptDialog->wasInterrupted())
	{
	    break;  // kick out of current task if user interrupts
	}
	
	for(j=0; j< 10000; j++)
	    k++;
    }

    // Task done, so we\'re not busy anymore

    theApplication->notBusy();    

    // Be nice to others, reset the application's busy dialog to the default

    theApplication->setBusyDialog(NULL);
}


void main ( int argc, char **argv )
{
    VkApp       *app = new VkApp("BusyApp", &argc, argv);

    BusyWindow  *win = new BusyWindow("Busy");
    BusyWindow  *win2 = new BusyWindow("Busy2");

    win->show();
    win2->show();

    app->run();
}




