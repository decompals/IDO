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


#include "MenuWindow.h"
#include "MenuBar.h"

void MenuWindow::initialize( )
{

    MainWindow::initialize();

    _menuBar = new MenuBar( _main, "menubar" );
        
    XtVaSetValues(_main, 
		  XmNmenuBar, _menuBar->baseWidget(),
		  NULL);

    createMenuPanes();

    _menuBar->manage();
}


MenuWindow::~MenuWindow()
{
    delete _menuBar;
}

