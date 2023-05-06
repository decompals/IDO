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
#include <stdio.h>
#include <stdlib.h>
#include "PhoneBook.h"
#include <Vk/VkApp.h>

static XrmOptionDescRec cmdOptions[] = {
{
    "-filename", "*defaultFilename", XrmoptionSepArg, NULL,
    },
};

void main(int argc, char **argv)
{
    PhoneBook *pb;
    VkApp *app;
    
    // Create the application, using the classname "PhoneBook" and processing
    // Xt's default set of command-line options.  Also process our own
    // command-line option, -filename.
    
    app = new VkApp("PhoneBook", &argc, argv, cmdOptions, XtNumber(cmdOptions));
    
    // Warn the user if there were any command-line options that were
    // not recognized
    
    if (app->argc() > 1)
    {
	fprintf(stderr, "%s: Illegal argument '%s'\n", app->argv(0), app->argv(1));
	fprintf(stderr, "Usage: PhoneBook [-filename database-file]\n");
	exit(1);
    }
    
    // Create the main PhoneBook window and show it
    
    pb = new PhoneBook("phoneBook");
    pb->show();
    
    // Go into the standard event loop
    
    app->run();
}
