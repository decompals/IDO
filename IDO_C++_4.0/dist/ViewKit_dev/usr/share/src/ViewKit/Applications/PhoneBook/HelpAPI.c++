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
//
// These routines satisfy the ViewKit on-line help hooks.  /usr/lib/libvkhelp.a
// also satisfies the hooks, but we want to do our own thing when the user
// selects on-line help.  In this case, we simple create and show our
// help dialog.  We ignore the context arguments since we only have one
// help window.
//

#include <Vk/VkHelp.h>
#include <Vk/VkApp.h>
#include "HelpDialog.h"

HelpDialog *helpDialog = NULL;

extern "C" {

int SGIHelpInit(Display *, char *, char *)
{
    return 1;
}

int SGIHelpMsg(char *, char *, char *)
{
    theApplication->busy();

    if (!helpDialog)
	helpDialog = new HelpDialog("helpDialog");

    helpDialog->show();
    helpDialog->open();
    helpDialog->raise();
    theApplication->notBusy();
    return 1;
}

int SGIHelpIndexMsg(char *, char *)
{
    SGIHelpMsg(NULL, NULL, NULL);
    return 1;
}
}
