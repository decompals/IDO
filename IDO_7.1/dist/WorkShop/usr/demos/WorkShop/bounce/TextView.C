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


 ///////////////////////////////////////////////////////////////
 // TextView.C: Abstract base class for ColorChooser text views
 ////////////////////////////////////////////////////////////////
 
#include "TextView.h"
 
#include <Xm/TextF.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
 
 TextView::TextView ( Widget parent, 
                     char  *name ) : ColorView ( name )
{
    _w = XtVaCreateWidget ( _name, 
                           xmRowColumnWidgetClass, 
                           parent, 
                           XmNorientation,    XmHORIZONTAL,
                           XmNpacking,        XmPACK_COLUMN,
                           XmNnumColumns,     3,
                           XmNentryAlignment, XmALIGNMENT_END,
                           XmNadjustLast,     FALSE,
                           NULL );
    
    installDestroyHandler(); 
    
    _label1 = XmCreateLabel ( _w, "label1", NULL, 0 );
    
    _field1 = XtVaCreateWidget ( "field1", xmTextFieldWidgetClass, _w,
                                XmNcolumns,     5,
                                XmNeditable,    FALSE,
                                XmNcursorPositionVisible, FALSE,
                                NULL );
    
    _label2 = XmCreateLabel ( _w, "label2", NULL, 0 );
    
    _field2 = XtVaCreateWidget ( "field2", xmTextFieldWidgetClass, _w,
                                XmNcolumns,     5,
                                XmNeditable,    FALSE,
                                XmNcursorPositionVisible, FALSE,
                                NULL );
    
    _label3 = XmCreateLabel ( _w, "label3", NULL, 0 );
    
    _field3 = XtVaCreateWidget ( "field3", xmTextFieldWidgetClass, _w,
                                XmNcolumns,     5,
                                XmNeditable,    FALSE,
                                XmNcursorPositionVisible, FALSE,
                                NULL );
    
    XtManageChild (_field1 );
    XtManageChild (_field2 );
    XtManageChild (_field3 );
    XtManageChild (_label1 );
    XtManageChild (_label2 );
    XtManageChild (_label3 );
}
