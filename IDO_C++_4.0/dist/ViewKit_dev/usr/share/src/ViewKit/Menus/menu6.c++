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


///////////////////////////////////////////////////////////////////
// This program demonstrates additional messages to menu items
//////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkMenu.h>
#include <Vk/VkMenuBar.h>
#include <Vk/VkSubMenu.h>
#include <Xm/Label.h>
#include <stdlib.h>

class MyWindow: public VkWindow {

  private:

    static void addOneCallback       ( Widget,  XtPointer , XtPointer);
    static void removeOneCallback    ( Widget,  XtPointer , XtPointer);
    static void activateOneCallback  ( Widget,  XtPointer , XtPointer);
    static void deactivateOneCallback( Widget,  XtPointer , XtPointer);
    static void sampleCallback       ( Widget,  XtPointer , XtPointer);
    static void quitCallback         ( Widget,  XtPointer , XtPointer);

  protected:

    VkSubMenu *_appMenuPane;
    VkSubMenu *_menuPaneTwo;

    void addOne();
    void removeOne();
    void activateOne();
    void deactivateOne();
    void sample();

    virtual Widget MyWindow::setUpInterface(Widget parent);

  public:
 
    MyWindow( const char *name) : VkWindow( name) { }

    virtual const char* className() { return "MyWindow";}
};

void MyWindow::addOneCallback( Widget,  XtPointer clientData , XtPointer)
{
    MyWindow *obj = ( MyWindow * ) clientData;

    obj->addOne();
}

void MyWindow::sampleCallback( Widget,  XtPointer clientData , XtPointer)
{
    MyWindow *obj = ( MyWindow * ) clientData;

    obj->sample();
}

void MyWindow::sample()
{


}

void MyWindow::removeOneCallback( Widget,  XtPointer clientData , XtPointer)
{
    MyWindow *obj = (MyWindow *) clientData;

    obj->removeOne();
}

void MyWindow::activateOneCallback( Widget,  XtPointer clientData , XtPointer)
{
    MyWindow *obj = (MyWindow *) clientData;

    obj->activateOne();
}

void MyWindow::deactivateOneCallback( Widget,  XtPointer clientData , XtPointer)
{
    MyWindow *obj = (MyWindow *) clientData;

    obj->deactivateOne();
}

void MyWindow::addOne()
{
    VkSubMenu *pane = (VkSubMenu* ) menu()->findNamedItem("Test");
    pane->addAction("A New Action", &MyWindow::sampleCallback, (XtPointer) this);
}

void MyWindow::removeOne()
{
   VkSubMenu *pane = (VkSubMenu* ) menu()->findNamedItem("Test");
    pane->removeItem("A New Action");
}

void MyWindow::activateOne()
{
   VkSubMenu *pane = (VkSubMenu* ) menu()->findNamedItem("Test");
   pane->activateItem("A New Action");
}

void MyWindow::deactivateOne()
{
   VkSubMenu *pane = (VkSubMenu* ) menu()->findNamedItem("Test");
    pane->deactivateItem("A New Action");
}

void MyWindow::quitCallback ( Widget, XtPointer, XtPointer )
{
    exit(0);
}

Widget MyWindow::setUpInterface(Widget parent)
{
    Widget label =  XmCreateLabel(parent, "a menu", NULL, 0);

    // Add a menu pane

    _appMenuPane = addMenuPane("Application");
    
    _appMenuPane->addAction("Add One",         &MyWindow::addOneCallback,        (XtPointer) this); 
    _appMenuPane->addAction("Remove One",      &MyWindow::removeOneCallback,     (XtPointer) this); 
    _appMenuPane->addAction("Activate One",    &MyWindow::activateOneCallback,   (XtPointer) this); 
    _appMenuPane->addAction("Deactivate One",  &MyWindow::deactivateOneCallback, (XtPointer) this); 
    _appMenuPane->addSeparator();
    _appMenuPane->addAction("Quit", &MyWindow::quitCallback, (XtPointer) this ); 

    // Add a menu second pane
    
    _menuPaneTwo = addMenuPane("PaneTwo");

    _menuPaneTwo->addSubmenu("Test");
    
    return label;	
}


main(int argc, char **argv)
{
  VkApp  *myApp = new VkApp("Menudemo3",  &argc,  argv);
  MyWindow  *menuWin  = new MyWindow("menuWindow");

  menuWin->show();

  myApp->run();
}



