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




////////////////////////////////////////////////////////////////
// Simple example of how to create a menubar in Vk
///////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkMenu.h>
#include <stream.h>
#include <Xm/Label.h>
#include <stdlib.h>

class MyWindow: public VkWindow {

  private:

    static void sampleCallback( Widget,  XtPointer , XtPointer);
    static void quitCallback( Widget,  XtPointer , XtPointer);

    void quit();
    void sample();

    static VkMenuDesc subMenu[];
    static VkMenuDesc sampleMenuPane[];
    static VkMenuDesc appMenuPane[];
    static VkMenuDesc mainMenuPane[];

  public:
 
    MyWindow( const char *name);
    ~MyWindow();

    virtual const char* className();
};

MyWindow::MyWindow( const char *name) : VkWindow( name)
{
    Widget label =  XmCreateLabel(mainWindowWidget(), "a menu", NULL, 0);
    
    setMenuBar(mainMenuPane);

    addView(label);
}

MyWindow::~MyWindow()
{
    // Empty
}

const char* MyWindow::className() 
{
    return "MyWindow";
}


// The menu bar is essentially a set of cascading menu panes, so the
// top level of the menu tree is always defined as a list of submenus

VkMenuDesc  MyWindow::mainMenuPane[] = {
  { SUBMENU, "Application",  NULL, MyWindow::appMenuPane},
  { SUBMENU, "Sample",       NULL, MyWindow::sampleMenuPane},
  { END}
};

VkMenuDesc MyWindow::appMenuPane[] = {
  { ACTION,   "One",     &MyWindow::sampleCallback},
  { ACTION,   "Two",     &MyWindow::sampleCallback},
  { ACTION,   "Three",   &MyWindow::sampleCallback},
  { SEPARATOR },
  { ACTION,   "Quit",    &MyWindow::quitCallback},
  { END},
};

VkMenuDesc MyWindow::sampleMenuPane[] = {
  { LABEL,    "Test Label"},
  { SEPARATOR },
  { ACTION,    "An Action",       &MyWindow::sampleCallback},
  { ACTION,    "Another Action",  &MyWindow::sampleCallback},
  { SUBMENU,   "A Submenu",       NULL, MyWindow::subMenu},
  { END},
};


VkMenuDesc MyWindow::subMenu[] = {
  { ACTION,  "foo",   &MyWindow::sampleCallback},
  { ACTION,  "bar",   &MyWindow::sampleCallback},
  { ACTION,  "baz",   &MyWindow::sampleCallback},
  { END},
};


void MyWindow::sample()
{
    cout << "sample callback" <<  "\n" << flush;
}

void MyWindow::sampleCallback( Widget,  XtPointer clientData  , XtPointer)
{
    MyWindow *obj = (MyWindow *) clientData;
    obj->sample();
}

void MyWindow::quitCallback ( Widget, XtPointer, XtPointer )
{
    exit(0);
}


void main(int argc, char **argv)
{
  VkApp     *myApp    = new VkApp("Menudemo",  &argc,  argv);
  MyWindow  *menuWin  = new MyWindow("MenuWindow");

  menuWin->show();

  myApp->run();
}

