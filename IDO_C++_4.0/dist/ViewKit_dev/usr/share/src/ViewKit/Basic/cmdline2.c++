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
// that adds additional command line arguments and loads them into
// a class data member
//////////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Xm/Label.h>
#include <stdio.h>

class MyApp : public VkApp {

  private:

    // Declare a description of the command line options

    static XrmOptionDescRec _cmdLineOptions[];

    // Declare a description of resources to be retieved

    static XtResource _resources[];

  protected:

    // A sample member function to be intialized from the
    // command line

    Boolean   _verbose;

  public:

    MyApp(char             *appClassName,
	  int              *arg_c, 
	  char            **arg_v,
	  XrmOptionDescRec *optionList       = NULL,
	  int               sizeOfOptionList = 0);
    ~MyApp();
};

// Describe the command line options

XrmOptionDescRec MyApp::_cmdLineOptions[] = 
{
    {
    "-verbose", "*verbose", XrmoptionNoArg, "TRUE",
    },
};


// Describe the reousrces to be retrieved and used to init the class

XtResource MyApp::_resources [] = {
{
     "verbose", 
     "Verbose", 
     XmRBoolean, 
     sizeof ( Boolean ),
     XtOffset ( MyApp *, _verbose ), 
     XmRString, 
     (XtPointer) "FALSE",
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

    // Parse the command line

    *arg_c = parseCommandLine(_cmdLineOptions, XtNumber(_cmdLineOptions));

    // Init this class fromthe resource data base

    getResources ( _resources, XtNumber(_resources) );
    
    printf("Verbose = %d\n", _verbose);
}

MyApp::~MyApp()
{
    // Empty
}


void main ( int argc, char **argv )
{
    MyApp *app = new MyApp("Cmdline2", &argc, argv);

    app->run();
}

