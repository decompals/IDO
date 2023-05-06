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

  public:
 
    MyWindow( const char *name);
    ~MyWindow();

    virtual const char* className();
};

void MyWindow::addOneCallback( Widget,  XtPointer clientData , XtPointer)
{
    MyWindow *obj = ( MyWindow * ) clientData;

    obj->addOne();
}

MyWindow::~MyWindow()
{
    // Empty
}

const char* MyWindow::className() { return "MyWindow";}

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
    _menuPaneTwo->addAction("A New Action", &MyWindow::sampleCallback, (XtPointer) this);
    _appMenuPane->activateItem("Remove One");
}

void MyWindow::removeOne()
{
    _menuPaneTwo->removeItem("A New Action");

    if(!_menuPaneTwo->findNamedItem("A New Action"))
      _appMenuPane->deactivateItem("Remove One");
}

void MyWindow::activateOne()
{
    _menuPaneTwo->activateItem("A New Action");
}

void MyWindow::deactivateOne()
{
    _menuPaneTwo->deactivateItem("A New Action");
}

void MyWindow::quitCallback ( Widget, XtPointer, XtPointer )
{
    exit(0);
}

MyWindow::MyWindow( const char *name) : VkWindow( name) 
{
    Widget label =  XmCreateLabel(mainWindowWidget(), "a menu", NULL, 0);

    // Add a menu pane

    _appMenuPane = addMenuPane("Application");
    _appMenuPane->showTearOff(FALSE);
    
    _appMenuPane->addAction("Add One",         &MyWindow::addOneCallback,        (XtPointer) this); 
    _appMenuPane->addAction("Remove One",      &MyWindow::removeOneCallback,     (XtPointer) this); 
    _appMenuPane->addAction("Activate One",    &MyWindow::activateOneCallback,   (XtPointer) this); 
    _appMenuPane->addAction("Deactivate One",  &MyWindow::deactivateOneCallback, (XtPointer) this); 
    _appMenuPane->addSeparator();
    _appMenuPane->addAction("Quit", &MyWindow::quitCallback, (XtPointer) this ); 

    // Add a menu second pane
    
    _menuPaneTwo = addMenuPane("PaneTwo");
    
    addView(label);	
}


void main(int argc, char **argv)
{
  VkApp  *myApp = new VkApp("Menudemo3",  &argc,  argv);
  MyWindow  *menuWin  = new MyWindow("menuWindow");

  menuWin->show();

  myApp->run();
}



