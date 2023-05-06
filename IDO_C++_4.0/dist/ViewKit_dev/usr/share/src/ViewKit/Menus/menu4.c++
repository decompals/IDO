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


////////////////////////////////////////////////////////////////////////////////
// Sample program that demonstrates how to create a popup menu
/////////////////////////////////////////////////////////////////
#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkPopupMenu.h>
#include <stream.h>
#include <Xm/Label.h>

class MyWindow: public VkWindow {

  private:

    VkPopupMenu *_popup;

    static void sampleCallback( Widget,  XtPointer , XtPointer);
    void sample();

    static VkMenuDesc subMenu[];
    static VkMenuDesc sampleMenuPane[];

  protected:


  public:
 
    MyWindow( const char *name);
    ~MyWindow();

    virtual const char* className();
};

MyWindow::MyWindow( const char *name) : VkWindow( name)
{
    Widget label =  XmCreateLabel(mainWindowWidget(), "a menu", NULL, 0);
	
    _popup = new VkPopupMenu(label, sampleMenuPane, (XtPointer) this);
    
    addView(label);	
}

MyWindow::~MyWindow( )
{

}

const char* MyWindow::className() { return "MyWindow";}

// The menu bar is essentially a set of cascading menu panes, so the
// top level of the menu tree is always defined as a list of submenus


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

void main(int argc, char **argv)
{
  VkApp     *myApp    = new VkApp("Menudemo",  &argc,  argv);
  MyWindow  *menuWin  = new MyWindow("MenuWindow");

  menuWin->show();

  myApp->run();
}

