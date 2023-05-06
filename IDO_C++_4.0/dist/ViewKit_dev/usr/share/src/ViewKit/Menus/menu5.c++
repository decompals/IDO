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
#include <Vk/VkPopupMenu.h>
#include <Vk/VkSubMenu.h>
#include <stream.h>
#include <Xm/Label.h>

class MyWindow: public VkWindow {

  private:

    VkPopupMenu *_popup;
    VkSubMenu   *_subMenu;

    static void sampleCallback( Widget,  XtPointer , XtPointer);

  protected:

    void sample();

  public:
 
    MyWindow( const char *name);
    ~MyWindow();

    virtual const char* className();
};


MyWindow::MyWindow( const char *name) : VkWindow( name)
{
    Widget label =  XmCreateLabel(mainWindowWidget(), "popup menu", NULL, 0);
    
    _popup = new VkPopupMenu();
    
    _popup->addLabel("Test Label");
    _popup->addSeparator();
    _popup->addAction("An Action", &MyWindow::sampleCallback, (XtPointer) this); 
    _subMenu = _popup->addSubmenu("A Submenu"); 
    
    _subMenu->addAction("foo", &MyWindow::sampleCallback, (XtPointer) this); 
    _subMenu->addAction("bar", &MyWindow::sampleCallback, (XtPointer) this); 
    _subMenu->addAction("baz", &MyWindow::sampleCallback, (XtPointer) this); 
    
    _popup->attach(label);
    
    addView(label);
}

MyWindow::~MyWindow()
{
    // Empty
}

const char* MyWindow::className() { return "MyWindow";}


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

