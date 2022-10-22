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
// AddEngineCmd.C
//////////////////////////////////////////////////////////

#include "AddEngineCmd.h"
#include "Engine.h"


AddEngineCmd::AddEngineCmd ( char *name, int active,  Stage *stage):
                   Cmd ( name, active )
{
    _stage = stage;
    _engine = NULL;
}

void AddEngineCmd::doit()
{
    _engine = new Engine(_stage);
}	

void AddEngineCmd::undoit()
{
    delete _engine;	
    _engine = NULL;
}	
