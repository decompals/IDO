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


#ifndef MENUWINDOW_H
#define MENUWINDOW_H

#include "MainWindow.h"

class MenuBar;

class MenuWindow : public MainWindow {

  protected:

    MenuBar *_menuBar;
    
    virtual void initialize();
    virtual void createMenuPanes() = 0;

  public:
    
    MenuWindow(char *name) : MainWindow (name) { }
    virtual ~MenuWindow();
};

#endif
