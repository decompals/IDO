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
// NoUndoCmd.C: Base class for all commands without undo
//////////////////////////////////////////////////////////

#include "NoUndoCmd.h"

#define NULL  0
#define TRUE  1
#define FALSE 0

NoUndoCmd::NoUndoCmd ( char  *name, 
                      int    active ) :
                      Cmd ( name, active )
{
    _hasUndo = FALSE; // This class has no undo
}

void NoUndoCmd::undoit()
{
    // Empty
}


