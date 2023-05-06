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

// This is a sample driver program based on the ViewKit.
// This program instantiates a VkApp and creates a VkWindow subclass.
// to be used to test the AlignmentTest class

#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkSubMenu.h>
#include "AlignmentTest.h"


class AlignedWindow: public VkWindow { 

  private:

        static void quitCallback(Widget, XtPointer, XtPointer); 

  protected:

        virtual Widget setUpInterface ( Widget ); 

  public:

    AlignedWindow ( const char *name ) : VkWindow ( name ) { }
    virtual const char* className() { return "AlignedWindow"; }
};

Widget AlignedWindow::setUpInterface ( Widget parent ) 
{ 
    // Instantiate the class we are testing 

    AlignmentTest* obj =  new AlignmentTest("sample", parent);

    // Add a quit menu item 

    VkSubMenu *pane = addMenuPane("File");

    pane->addAction("Quit", &AlignedWindow::quitCallback, (XtPointer) this);

    // Display the component

    obj->show();

    return obj->baseWidget();
}

void AlignedWindow::quitCallback ( Widget, XtPointer clientData, XtPointer ) 
{
    AlignedWindow *obj = (AlignedWindow*) clientData;
    delete obj;
}


class UnalignedWindow: public VkWindow { 

  private:

        static void quitCallback(Widget, XtPointer, XtPointer); 

  protected:

        virtual Widget setUpInterface ( Widget ); 

  public:

    UnalignedWindow ( const char *name ) : VkWindow ( name ) { }
    virtual const char* className() { return "UnalignedWindow"; }
};

Widget UnalignedWindow::setUpInterface ( Widget parent ) 
{ 
    // Instantiate the class we are testing 

    AlignmentTestUI* obj =  new AlignmentTestUI("sample", parent);

    // Add a quit menu item 

    VkSubMenu *pane = addMenuPane("File");

    pane->addAction("Quit", &UnalignedWindow::quitCallback, (XtPointer) this);

    // Display the component

    obj->show();

    return obj->baseWidget();
}

void UnalignedWindow::quitCallback ( Widget, XtPointer clientData, XtPointer ) 
{
    UnalignedWindow *obj = (UnalignedWindow*) clientData;
    delete obj;
}


void main ( int argc, char **argv )
{
    VkApp              *sampleApp       = new VkApp("AlignmentTest", &argc, argv);
    AlignedWindow      *alignedWindow   = new AlignedWindow("aligned");
    UnalignedWindow  *unalignedWindow = new UnalignedWindow("unaligned");

    alignedWindow->show();
    unalignedWindow->show();

    sampleApp->run();
}
