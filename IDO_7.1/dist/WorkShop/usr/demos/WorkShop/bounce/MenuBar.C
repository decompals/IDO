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
// MenuBar.C
//////////////////////////////////////////////////////////

#include "MenuBar.h"
#include "Cmd.h"
#include "CmdList.h"
#include "ButtonInterface.h"

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeBG.h>

MenuBar::MenuBar ( Widget parent, char *name ) : UIComponent ( name )
{
    // The base widget is a Motif menu bar widget
    
    _w = XmCreateMenuBar ( parent, _name, NULL, 0 );
    
    installDestroyHandler();
}

void MenuBar::addCommands ( CmdList *list, char *name )
{
    int    i;
    Widget pulldown, cascade;
    
    // Create a pulldown menu pane for this list of commands
    
    pulldown = XmCreatePulldownMenu ( _w, name, NULL, 0 );
    
    // Each entry in the menu bar must have a cascade button
    // from which the user can pull down the pane
    
    cascade = XtVaCreateWidget ( name, 
                                xmCascadeButtonGadgetClass,
                                _w, 
                                XmNsubMenuId, pulldown, 
                                NULL );
    XtManageChild(cascade);
    
    // Loop through the cmdList, creating a menu entry for each 
    // command. 
    
    for ( i = 0; i < list->size(); i++)
    {
        CmdInterface *ci;
        ci  = new ButtonInterface ( pulldown, (*list)[i] );
    }
}

void MenuBar::addHelpPane ( CmdList *list, char *name )
{
    int    i;
    Widget pulldown, cascade;
    
    // Create a pulldown menu pane for this list of commands
    
    pulldown = XmCreatePulldownMenu ( _w, name, NULL, 0 );
    
    // Each entry in the menu bar must have a cascade button
    // from which the user can pull down the pane
    
    cascade = XtVaCreateWidget ( name, 
                                xmCascadeButtonGadgetClass,
                                _w, 
                                XmNsubMenuId, pulldown, 
                                NULL); 
    
    // Specify this pane's cascade button as the help widget
    
    XtVaSetValues (_w, 
                   XmNmenuHelpWidget, cascade, 
                   NULL ); 
    
    // Loop through the cmdList, creating a menu entry for each 
    // command. 
    
    for ( i = 0; i < list->size(); i++)
    {
        CmdInterface *ci;
        ci = new ButtonInterface ( pulldown, (*list)[i] );
    }
}       

