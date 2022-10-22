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


#include <Xm/Xm.h>
#include <Xm/PushBG.h>
#include "ButtonInterface.h"
#include "Cmd.h"

ButtonInterface::ButtonInterface(Widget parent, 
                                 Cmd *cmd) : CmdInterface( cmd)
{
    _w = XtCreateManagedWidget ( _name, 
                                xmPushButtonGadgetClass,
                                parent,
                                NULL, 0 );
    
    if(_active)
        activate();     
    else
        deactivate();   
    
    XtAddCallback ( _w,  
                   XmNactivateCallback, 
                   &CmdInterface::executeCmdCallback,
                   (XtPointer) this );  
}

