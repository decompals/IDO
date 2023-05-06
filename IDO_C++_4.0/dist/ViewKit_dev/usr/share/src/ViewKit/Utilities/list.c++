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

/////////////////////////////////////////////////////////////////////
// list.c++: This example displays a list widget in a window. 
//           The contents of the list are constructed using
//           a VkNameList object.
////////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Xm/List.h>
#include <Vk/VkNameList.h>

// Define a top-level window class

class MyWindow: public VkSimpleWindow {

  protected:

    Widget _list;    // Hang on to widget a a data member

  public:

    MyWindow ( const char *name );
    ~MyWindow();
    virtual const char* className();  // Identify this class
};


// The MyWindow constructor provides a place in which to create a
// widget tree to be installed as a "view" in the window.

MyWindow::MyWindow ( const char *name ) : VkSimpleWindow ( name ) 
{
    _list =  XmCreateList ( mainWindowWidget(), "list", NULL, 0 );

    // Create a name list object

    VkNameList *items = new VkNameList();

    // Add some items

    items->add("One");
    items->add("Two");
    items->add("Three");
    items->add("Four");
    items->add("One");

    items->removeDuplicates();  // Get rid of any duplications
    items->sort();              // sort the list
    items->reverse();           // Now reverse it

    // Display the items in the list widget

    XtVaSetValues(_list, 
		  XmNitems, (XmStringTable) (*items),
		  XmNitemCount, items->size(),
		  NULL);

    addView(_list);
}

const char * MyWindow::className()
{
    return "MyWindow";
}      

MyWindow::~MyWindow()
{
    // Empty
}


// Main driver. Just instantiate a VkApp and a top-level window, "show" 
// the window and then "run" the application.

void main ( int argc, char **argv )
{
    VkApp     *app = new VkApp("Hello", &argc, argv);
    MyWindow  *win = new MyWindow("hello");

    win->show();
    app->run();
}




