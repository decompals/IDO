///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//         This example code is from the book:
//
//           Object-Oriented Programming with C++ and OSF/Motif
//         by
//           Douglas Young
//           Prentice Hall, 1992
//           ISBN 0-13-630252-1	
//
//         Copyright 1991 by Prentice Hall
//         All Rights Reserved
//
//  Permission to use, copy, modify, and distribute this software for 
//  any purpose except publication and without fee is hereby granted, provided 
//  that the above copyright notice appear in all copies of the software.
///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////
// MainWindow.C: Support a toplevel window
////////////////////////////////////////////////////////////////////
#include "Application.h"
#include "MainWindow.h"
#include <Xm/MainW.h>
#include <assert.h>

MainWindow::MainWindow(char *name) : UIComponent(name)
{
    _workArea = NULL;
    assert ( theApplication );    
    theApplication->registerWindow(this);
}


void MainWindow::initialize( )
{
    
    // All toplevel windows in the MotifApp framework are 
    // implemented as a popup shell off the ApplicationŐs
    // base widget.
    
    _w = XtCreatePopupShell ( _name, 
			      applicationShellWidgetClass,
			      theApplication->baseWidget(),
			      NULL, 0 );
    
    installDestroyHandler();
    
    // Use a Motif XmMainWindow widget to handle window layout
    
    _main = XtCreateManagedWidget ( "mainWindow", 
				    xmMainWindowWidgetClass,
				    _w, 
				    NULL, 0 );
    
    // Called derived class to create the work area
    
    _workArea = createWorkArea ( _main );  
    assert ( _workArea );
    
    // Designate the _workArea widget as the XmMainWindow
    // widget's XmNworkArea widget
    
    XtVaSetValues ( _main, 
		    XmNworkWindow, _workArea, 
		    NULL );
    
    // Manage the work area if the derived class hasnŐt already.
    
    if ( !XtIsManaged ( _workArea ) )
	XtManageChild ( _workArea ); 
}

MainWindow::~MainWindow()
{
    // Unregister this window with the Application object
    
    theApplication->unregisterWindow(this);
}

void MainWindow::manage()
{
    assert ( _w );
    XtPopup ( _w, XtGrabNone);
    
    if(XtIsRealized(_w))
	XMapRaised(XtDisplay(_w), XtWindow(_w));
}

void MainWindow::unmanage()
{
    assert ( _w );
    XtPopdown ( _w );
}


void MainWindow::iconify()
{
    assert ( _w );
    
    if(XtIsRealized(_w))
	XIconifyWindow(XtDisplay(_w), XtWindow(_w), 0);
}



