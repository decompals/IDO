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

/////////////////////////////////////////////////////////////
// BounceWindow.h: 
/////////////////////////////////////////////////////////////

#include "MenuWindow.h"

class Clock;
class Stage;
class ControlPanel;

class BounceWindow : public MenuWindow {

 private:

    Clock *_clock;
    Stage *_stage;
    ControlPanel *_controlPanel;

 protected:

    virtual Widget createWorkArea(Widget);
    virtual void   createMenuPanes();

 public:

    BounceWindow(char *);
};

