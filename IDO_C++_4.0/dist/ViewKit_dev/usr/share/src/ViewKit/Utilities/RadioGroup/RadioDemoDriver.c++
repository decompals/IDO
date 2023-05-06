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
#include <Vk/VkWindow.h>
#include <Vk/VkSubMenu.h>
#include "RadioGroupTest.h"


class SampleWindow: public VkWindow { 

  private:

        static void quitCallback(Widget, XtPointer, XtPointer); 

  protected:

        virtual Widget setUpInterface ( Widget ); 

  public:

    SampleWindow ( const char *name ) : VkWindow ( name ) { }
    virtual const char* className() { return "SampleWindow"; }
};

Widget SampleWindow::setUpInterface ( Widget parent ) 
{ 
    // Instantiate the class we are testing 

    RadioGroupTest* obj =  new RadioGroupTest("sample", parent);

    // Add a quit menu item 

    VkSubMenu *pane = addMenuPane("File");

    pane->addAction("Quit", &SampleWindow::quitCallback, (XtPointer) this);

    // Display the component

    obj->show();

    return obj->baseWidget();
}

void SampleWindow::quitCallback ( Widget, XtPointer, XtPointer ) 
{
    theApplication->quitYourself();
}


void main ( int argc, char **argv )
{
    VkApp        *sampleApp = new VkApp("RadioDemo", &argc, argv);
    SampleWindow  *sampleWin = new SampleWindow("RadioDemo");

    sampleWin->show();
    sampleApp->run();
}
