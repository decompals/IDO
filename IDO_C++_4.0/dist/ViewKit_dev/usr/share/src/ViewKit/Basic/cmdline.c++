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

///////////////////////////////////////////////////////////////////////
// Example program that demonstrates how to create a VkApp subclass
// that adds additional command line arguments
//////////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkResource.h>
#include <iostream.h>

class MyApp : public VkApp {

  private:

    // Declare the structure that describes the options as 
    // part of this class

    static XrmOptionDescRec cmdLineOptions[];
  
  public:

    MyApp(char             *appClassName,
	  int              *arg_c, 
	  char            **arg_v,
	  XrmOptionDescRec *optionList       = NULL,
	  int               sizeOfOptionList = 0);
  ~MyApp();
};


MyApp::~MyApp()
{
    // Empty
}

// The structure that describes the options

XrmOptionDescRec MyApp::cmdLineOptions[] = {
    {
    "-verbose", "*verbose", XrmoptionNoArg, "TRUE",
    },
};

MyApp::MyApp(char             *appClassName,
	     int              *arg_c, 
	     char            **arg_v,
	     XrmOptionDescRec *optionList,
	     int               sizeOfOptionList) :   VkApp(appClassName,
							   arg_c, 
							   arg_v,
							   optionList,
							   sizeOfOptionList) 
{

    // Call base class function to parse command line. Note
    // assignment to *arg_c. Necessary if caller's argc is to be
    // correct upon return.

    *arg_c = parseCommandLine(cmdLineOptions, XtNumber(cmdLineOptions));

    char *mode = VkGetResource("verbose", "Verbose");

    cerr << "Verbose Mode =" << (mode ? mode : "false") << "\n";
}


void main ( int argc, char **argv )
{
    MyApp  *app = new MyApp("HelloApp", &argc, argv);

    app->run();
}



