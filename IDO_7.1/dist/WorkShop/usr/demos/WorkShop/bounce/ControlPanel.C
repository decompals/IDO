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
// ControlPanel.C
//////////////////////////////////////////////////////////

#include "ControlPanel.h"
#include "ButtonInterface.h"
#include "Clock.h"
#include "RunCmd.h"
#include "StopCmd.h"
#include "StepCmd.h"
#include <Xm/RowColumn.h>


ControlPanel::ControlPanel ( Widget parent, char *name, Clock *clock ) : UIComponent ( name )
{
    CmdInterface *runBtn, *stopBtn, *stepBtn;
    Cmd          *runCmd, *stopCmd, *stepCmd;

    _w = XtVaCreateManagedWidget ( _name, xmRowColumnWidgetClass, parent, 
				  XmNnumColumns, 1,
				  XmNorientation, XmHORIZONTAL, 
				  NULL );

    installDestroyHandler();

    runCmd = new  RunCmd  ( "Run",  TRUE,  clock );
    stopCmd = new StopCmd ( "Stop", FALSE, clock );
    stepCmd = new StepCmd ( "Step", TRUE,  clock );

    runCmd->addToActivationList ( stopCmd );
    runCmd->addToDeactivationList ( stepCmd );
    runCmd->addToDeactivationList ( runCmd );

    stopCmd->addToActivationList ( runCmd );
    stopCmd->addToActivationList ( stepCmd );
    stopCmd->addToDeactivationList ( stopCmd );

    stepCmd->addToActivationList ( runCmd );
    stepCmd->addToDeactivationList ( stopCmd );

    runBtn   = new ButtonInterface ( _w, runCmd );   
    stopBtn  = new ButtonInterface ( _w, stopCmd );
    stepBtn  = new ButtonInterface ( _w, stepCmd );
}


