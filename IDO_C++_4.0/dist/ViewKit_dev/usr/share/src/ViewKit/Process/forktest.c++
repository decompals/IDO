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
#include <Vk/VkForkWindow.h>
#include <iostream.h>
#include <stdio.h>

VkForkIO *forkIO = NULL;

/**********************************************************************/

#define COMMAND "/usr/bin/bc"

static void buttonPressCallback(Widget, XtPointer, XtPointer)
{
    forkIO->outputString("\n");
    forkIO->outputString("Executing: ");
    forkIO->outputString(COMMAND);
    forkIO->outputString("\n");
    forkIO->outputString("\n");
    forkIO->execCommand(COMMAND);
}

static void exitCallback(VkCallbackObject *, void *, void * callData)
{
    int status  = (int) callData;

    fprintf(stderr, "Process exited with status %d\n", status);
}

static void forkioCallback(VkCallbackObject *, void *clientData, void * callData)
{
    VkForkIOReason reason  = (VkForkIOReason) callData;
    VkForkWindow  *win = (VkForkWindow *) clientData;

    if (reason == FIO_destroyed)
    {
	fprintf(stderr, "Window destroyed\n");
    }
    else if (reason == FIO_ctrlc) 
    {
	fprintf(stderr, "Process killed\n");
    }
}

void main(int argc, char **argv)
{
    VkApp     *app = new VkApp("Forktest", &argc, argv);

    VkForkWindow *win = new VkForkWindow("forkIOTest");

    forkIO = win->forkIO();

    VkAddCallbackFunction(VkForkIO::inputCallback, forkIO, forkioCallback, (void *) win);
    VkAddCallbackFunction(VkForkIO::exitCallback, forkIO, exitCallback, (void *) win);

    XtVaSetValues(forkIO->textWidget(), 
		  XmNrows,    10, 
		  XmNcolumns, 80,
		  NULL);
    win->show();

    buttonPressCallback(0, 0, 0);

    app->run();
}



