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
// AddBallCmd.h
//////////////////////////////////////////////////////////
#ifndef ADDBALLCMD_H
#define ADDBALLCMD_H

#include "Cmd.h"

class Stage;
class Actor;

class AddBallCmd : public Cmd {

  protected:

    Stage   *_stage;
    Actor   *_ball;
    char    *_color;

    virtual void doit();
    virtual void undoit();

   public:

    AddBallCmd ( char *, int active, Stage*, char *color = (char *) 0 );
};

#endif
