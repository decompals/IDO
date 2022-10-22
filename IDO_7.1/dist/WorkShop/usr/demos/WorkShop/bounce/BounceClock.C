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

///////////////////////////////////////////////////////////////////
// BounceClock.C: The clock that controls the animation in bounce
//////////////////////////////////////////////////////////////////
#include "Application.h"
#include "BounceClock.h"
#include "Stage.h"


BounceClock::BounceClock(Widget parent, char *name, Stage *stage) :
	Clock (parent, name, 0, 30 )
{
    _stage = stage;
}

void BounceClock::tick()
{
    _stage->nextFrame();
}
