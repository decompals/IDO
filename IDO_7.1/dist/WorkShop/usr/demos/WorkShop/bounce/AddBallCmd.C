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
// AddBallCmd.C
//////////////////////////////////////////////////////////

#include "AddBallCmd.h"
#include "BouncingBall.h"


AddBallCmd::AddBallCmd ( char *name, int active,  Stage *stage,
			char *color ) : Cmd ( name, active )
{
    _stage = stage;
    _name = name;
    _color = color;	
    _ball = NULL;
}

void AddBallCmd::doit()
{
    _ball = new BouncingBall(_stage, _color);
}	

void AddBallCmd::undoit()
{
    delete _ball;	
    _ball = NULL;
}	
