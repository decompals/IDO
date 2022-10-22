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
// RunCmd.h
//////////////////////////////////////////////////////////

#ifndef RUNCMD_H
#define RUNCMD_H

#include "Cmd.h"
class    Clock;

class RunCmd : public Cmd {

  protected:

    Clock   *_clock;

    virtual void doit();
    virtual void undoit();
	
  public:

    RunCmd ( char *, int, Clock * );

};
#endif
