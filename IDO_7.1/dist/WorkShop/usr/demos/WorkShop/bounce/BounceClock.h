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

///////////////////////////////////////////////////////////////
// BounceClock.h
///////////////////////////////////////////////////////////////
    
#ifndef BOUNCECLOCK_H
#define BOUNCECLOCK_H
    
#include "Clock.h"

class Stage;

class BounceClock : public Clock {

  private:

    Stage *_stage;

  protected:

    virtual void tick();

  public:

    BounceClock(Widget, char *, Stage *stage);    

    virtual const char *const className() { return "BounceClock"; };
};
#endif
