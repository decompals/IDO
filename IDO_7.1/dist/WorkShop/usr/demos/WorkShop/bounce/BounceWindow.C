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
// BounceWindow.C
//////////////////////////////////////////////////////////

#include "Application.h"
#include "BounceWindow.h"
#include "Stage.h"
#include "ControlPanel.h"
#include "BounceClock.h"
#include "QuitCmd.h"
#include "UndoCmd.h"
#include "CmdList.h"
#include "AddBallCmd.h"
#include "AddEngineCmd.h"
#include "MenuBar.h"
#include <Xm/Form.h>
#include <Xm/Separator.h>


BounceWindow::BounceWindow ( char *name ) : MenuWindow (name)
{
    _clock         = NULL;
    _stage         = NULL;
    _controlPanel  = NULL;
}

Widget BounceWindow::createWorkArea (Widget parent)
{

    Widget form =  XtCreateManagedWidget ( "workArea", 
					   xmFormWidgetClass,
					   parent,
					   NULL, 0 );

    _stage        = new Stage (form, "stage" );
    _clock        = new BounceClock(form, "clock", _stage);
    _controlPanel = new ControlPanel ( form, "control", _clock );


    XtVaSetValues ( _controlPanel->baseWidget(),
		   XmNtopWidget,        _clock->baseWidget(),
		   XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
		   XmNleftAttachment,   XmATTACH_FORM,
		   XmNrightPosition,    50,
		   XmNrightAttachment,  XmATTACH_POSITION,
		   XmNbottomAttachment, XmATTACH_NONE,
		   (char*)NULL );

    XtVaSetValues ( _clock->baseWidget(), 
		   XmNtopAttachment,    XmATTACH_NONE,
		   XmNleftPosition,     50,
		   XmNleftAttachment,   XmATTACH_POSITION,
		   XmNrightAttachment,  XmATTACH_FORM,
		   XmNbottomAttachment, XmATTACH_FORM,
		   (char*)NULL );


    Widget sep =  XtVaCreateManagedWidget ( "sep", 
					   xmSeparatorWidgetClass,
					   form,
					   XmNleftAttachment,   XmATTACH_FORM,
					   XmNrightAttachment,  XmATTACH_FORM,
					   XmNtopAttachment,    XmATTACH_NONE,
					   XmNbottomWidget,     _clock->baseWidget(),
					   XmNbottomAttachment, XmATTACH_WIDGET,
					   NULL );

    
    XtVaSetValues ( _stage->baseWidget(),
		   XmNtopAttachment,    XmATTACH_FORM,
		   XmNleftAttachment,   XmATTACH_FORM,
		   XmNrightAttachment,  XmATTACH_FORM,
		   XmNbottomWidget,     sep,
		   XmNbottomAttachment, XmATTACH_WIDGET,
		   XmNheight,           200,
		   (char*)NULL );

    _controlPanel->manage();
    _stage->manage();
    _clock->manage();    

    return form;	
}


void BounceWindow::createMenuPanes ()
{
    // Create the main application menu, with just a quit and undo cmd

    CmdList *cmdList  = new CmdList();
    Cmd     *quit     = new QuitCmd ( "Quit", TRUE );
    cmdList->add(theUndoCmd);        
    cmdList->add(quit);
    _menuBar->addCommands(cmdList, "Application" );

    // Create a menu for adding actors to the screen
    
    CmdList *figureList = new CmdList();

    Cmd *addRed   = new AddBallCmd ( "Add Red Ball",   TRUE, _stage,  "Red"  );    
    Cmd *addGreen = new AddBallCmd ( "Add Green Ball", TRUE, _stage, "Green" );    
    Cmd *addBlue  = new AddBallCmd ( "Add Blue Ball",  TRUE, _stage, "Blue"  );    
    Cmd *addAny   = new AddBallCmd ( "Add Ball...",    TRUE, _stage );
    Cmd *e        = new AddEngineCmd ( "Add Engine",   TRUE, _stage );        

    figureList->add(addRed);
    figureList->add(addGreen);
    figureList->add(addBlue);
    figureList->add(addAny);
    figureList->add(e);    

    _menuBar->addCommands(figureList, "Actors");
}
