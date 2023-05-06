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


////////////////////////////////////////////////////////////////////
// Demostrate viewkit interface to option menus
///////////////////////////////////////////////////////////////////
#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkOptionMenu.h>
#include <stream.h>
#include <Xm/RowColumn.h>

class MyWindow: public VkSimpleWindow {

  private:

    static void sampleCallback( Widget,  XtPointer , XtPointer);

    static VkMenuDesc MyWindow::optionPaneDesc[];

  protected:

    void sample(Widget, XtPointer);
    VkOptionMenu *_optionMenu;

  public:
 
    MyWindow( const char *name);
    ~MyWindow( );

    virtual const char* className();
};

VkMenuDesc MyWindow::optionPaneDesc[] = {
  { ACTION,   "Red",   &MyWindow::sampleCallback},
  { ACTION,   "Green", &MyWindow::sampleCallback},
  { ACTION,   "Blue",  &MyWindow::sampleCallback},
  { END},
};


MyWindow::MyWindow( const char *name) : VkSimpleWindow( name)
{
    Widget rc = XmCreateRowColumn(mainWindowWidget(), "rc", NULL, 0);

    _optionMenu = new VkOptionMenu(rc, optionPaneDesc, (XtPointer) this);
    _optionMenu->set("Green");

    addView(rc);
}

MyWindow::~MyWindow( )
{

}

const char* MyWindow::className() { return "MyWindow";}

void MyWindow::sampleCallback( Widget w, XtPointer clientData, XtPointer callData)
{
    MyWindow *obj = (MyWindow *) clientData;
    obj->sample(w, callData);
}

void MyWindow::sample(Widget, XtPointer)
{
    cout << "Selected item's index = " 
	 << _optionMenu->getIndex() 
	 << ",  name = " 
	 << _optionMenu->getItem()->name() 
	 << "\n"
	 << flush;
}

void main(int argc, char **argv)
{
  VkApp     *app    = new VkApp("Option",  &argc,  argv);
  MyWindow  *win  = new MyWindow("OptionMenu");

  win->show();

  app->run();
}

