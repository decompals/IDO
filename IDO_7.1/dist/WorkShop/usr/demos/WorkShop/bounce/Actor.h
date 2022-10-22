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

/////////////////////////////////////////////////////////////////////////////
// Actor.h: Abstract base class for all "actor" objects
////////////////////////////////////////////////////////////////////////////

#ifndef ACTOR_H
#define ACTOR_H

#include <Xm/Xm.h>

class Stage;

class Actor {

  protected:

    Stage *_stage;

  public:

    Actor( Stage *);

    virtual ~Actor();

    virtual void nextFrame(Drawable, Dimension, Dimension) = 0;
};

typedef Actor* ActorPtr;

#endif
