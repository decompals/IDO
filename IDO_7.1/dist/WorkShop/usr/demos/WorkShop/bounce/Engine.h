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


class Engine : public Actor {

  protected:

    GC gcPiston, gcShaft, gcCylinder,
       gcRoter, gcBack,
       gcDep, gcPre, gcEngine;

  public:

    Engine( Stage *);

    void nextFrame(Drawable, Dimension, Dimension);

    void reset();
};

#endif
