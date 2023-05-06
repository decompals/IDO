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


////////////////////////////////////////////////////////////////////////
// This program creates the same menubar as the menu.c++ demo but does
// it by dynamically creating menu objects.
/////////////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkSubMenu.h>
#include <Vk/VkRadioSubMenu.h>
#include <Vk/VkMenu.h>
#include <Xm/Label.h>
#include <stream.h>
#include <stdlib.h>

class MyWindow: public VkWindow {

  private:

    static void sampleCallback( Widget,  XtPointer , XtPointer);
    static void quitCallback( Widget,  XtPointer , XtPointer);

  protected:

    void sample();

  public:
 
    MyWindow( const char *name);
    ~MyWindow();

    virtual const char* className();
};


MyWindow::~MyWindow()
{
    // Empty
}

void MyWindow::sampleCallback( Widget,  XtPointer clientData , XtPointer)
{
    MyWindow *obj = (MyWindow *) clientData;
    obj->sample();
}

const char* MyWindow::className() { return "MyWindow";}

void MyWindow::sample()
{

    cout << "In Sample Callback" << "\n" << flush;
}

void MyWindow::quitCallback ( Widget, XtPointer, XtPointer )
{
    exit(0);
}

MyWindow::MyWindow( const char *name) : VkWindow( name) 
{
    Widget label =  XmCreateLabel(mainWindowWidget(), "a menu", NULL, 0);

    // Add a menu pane

    VkSubMenu *appMenuPane = addMenuPane("Application");
    
    appMenuPane->addAction("One",  &MyWindow::sampleCallback, (XtPointer) this); 
    appMenuPane->addAction("Two",  &MyWindow::sampleCallback, (XtPointer) this); 
    appMenuPane->addSeparator();
    appMenuPane->addAction("Quit", &MyWindow::quitCallback, (XtPointer) this); 

    // Add a menu second pane
    
    VkSubMenu *sampleMenuPane = addMenuPane("Sample");
    
    sampleMenuPane->addLabel("Test Label");
    sampleMenuPane->addSeparator();
    sampleMenuPane->addAction("An Action",      &MyWindow::sampleCallback, (XtPointer) this); 

    // Create a cascading submenu

    VkRadioSubMenu *subMenu = sampleMenuPane->addRadioSubmenu("A Submenu"); 
    
    subMenu->addToggle("foo", &MyWindow::sampleCallback, (XtPointer) this); 
    subMenu->addToggle("bar", &MyWindow::sampleCallback, (XtPointer) this); 
    subMenu->addToggle("baz", &MyWindow::sampleCallback, (XtPointer) this); 
    
    addView(label);
}


void main(int argc, char **argv)
{
    VkApp  *myApp = new VkApp("Menu",  &argc,  argv);
    MyWindow  *w1  = new MyWindow("menuwindow");

    w1->show();
    
    myApp->run();
}



