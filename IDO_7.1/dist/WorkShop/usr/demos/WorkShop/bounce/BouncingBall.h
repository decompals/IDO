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

//////////////////////////////////////////////////////////
// BouncingBall.h
//////////////////////////////////////////////////////////
#ifndef BOUNCINGBALL_H
#define BOUNCINGBALL_H

#include "Actor.h"
#include "Xm/Xm.h"

class Stage;

struct Point {
  int x, y;
};

class BouncingBall : public Actor {

  protected:

    GC         _gc;
    Point      _delta;
    XRectangle _bounds;

    static void colorSelectedCallback ( int red, int green, int blue , void *clientData);
    static void canceledCallback ( void *);

    void colorSelected ( int red, int green, int blue);

  public:

    BouncingBall( Stage *, char *);

    void nextFrame(Drawable, Dimension, Dimension);

    void reset();
};

#endif
