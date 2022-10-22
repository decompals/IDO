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
// Clock
///////////////////////////////////////////////////////////////
    
#ifndef CLOCK_H
#define CLOCK_H
    
#include "UIComponent.h"

class Clock : public UIComponent {

  private:

    int          _delta;
    long         _lastTime;

    Boolean      _stopped;

    XtIntervalId _id;

    virtual void timeout();
    virtual void speedChanged(int);

    static void timeoutCallback(XtPointer, XtIntervalId *);
    static void speedChangedCallback(Widget, XtPointer, XtPointer);
    
  protected:

    virtual void tick() = 0;

  public:

    Clock(Widget, char *, int min, int max);    

    void stop();
    void pulse();
    void start();

    virtual const char *const className() { return "Clock"; };
};
#endif
