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


///////////////////////////////////////////////////////////
// DialogManager.C: Support cached dialog widgets
//////////////////////////////////////////////////////////

#include "DialogManager.h"
#include "Application.h"
#include <Xm/MessageB.h>
#include <assert.h>

DialogManager::DialogManager ( char   *name ):
	UIComponent ( name )
{
    
}

Widget DialogManager::post ( char          *text,
                            void          *clientData,
                            DialogCallback ok,
                            DialogCallback cancel,
                            DialogCallback help)
  
{
    // Get a dialog widget from the cache
    
    Widget dialog = getDialog();
    
    // Convert the text string to a compound string and 
    // specify this to be the message displayed in the dialog.
    
    XmString xmstr = XmStringCreateSimple ( text ); 
    XtVaSetValues ( dialog, XmNmessageString, xmstr, NULL );
    XmStringFree ( xmstr );
    
    DialogCallbackData *dcb = new DialogCallbackData( this, 
                                                      clientData,
                                                      ok,
						      cancel, help );
    
    // Install callback function for each button 
    // support by Motif dialogs. If there is no help callback
    // unmanage the corresponding button instead, if possible.

    XtAddCallback ( dialog, 
                    XmNokCallback, 
                    &DialogManager::okCallback,
                    (XtPointer) dcb );
    
    XtAddCallback ( dialog, 
                    XmNcancelCallback, 
                    &DialogManager::cancelCallback,
                     (XtPointer) dcb );
  
    if ( help )	    
	        XtAddCallback ( dialog, 
		                        XmNhelpCallback, 
		                       &DialogManager::helpCallback,
		                       (XtPointer) dcb );
    else if ( XtIsSubclass ( dialog, xmMessageBoxWidgetClass ) )
    {
        Widget w = XmMessageBoxGetChild ( dialog,
                                          XmDIALOG_HELP_BUTTON );
	XtUnmanageChild ( w );
    }
    
    // Post the dialog.
    
    XtManageChild ( dialog );
    
    return dialog;
}

void DialogManager::okCallback ( Widget w, 
                                XtPointer clientData,
                                XtPointer )
{
    DialogCallbackData *dcd = (DialogCallbackData *) clientData;
    DialogManager *obj = (DialogManager *) dcd->dialogManager();
    DialogCallback callback;
    
    // If caller specified an ok callback, call the function

    if ( ( callback = dcd->ok() ) != NULL )
        ( *callback )( dcd->clientData() );
    
    // Reset for the next time

    obj->cleanup ( w, dcd );    
}

void DialogManager::cancelCallback ( Widget    w, 
                                      XtPointer clientData,
                                      XtPointer )
{
    DialogCallbackData *dcd = (DialogCallbackData *) clientData;
    DialogManager      *obj = (DialogManager *) dcd->dialogManager();
    DialogCallback callback;

    if ( ( callback = dcd->cancel() ) != NULL )
        ( *callback )( dcd->clientData() );
    
    obj->cleanup ( w, dcd );
}
 
void DialogManager::helpCallback ( Widget    w, 
				  XtPointer clientData,
				  XtPointer )
{
     DialogCallbackData *dcd = (DialogCallbackData *) clientData;
     DialogManager      *obj = (DialogManager *) dcd->dialogManager();
     DialogCallback callback;
 
     if ( ( callback = dcd->help() ) != NULL )
         ( *callback )( dcd->clientData() );
     
     obj->cleanup ( w, dcd );
}


void DialogManager::cleanup( Widget w, DialogCallbackData *dcd )
{
    
    XtRemoveCallback ( w, 
                      XmNokCallback, 
                      &DialogManager::okCallback,
                      (XtPointer) dcd );
    
    XtRemoveCallback ( w, 
                      XmNcancelCallback, 
                      &DialogManager::cancelCallback,
                      (XtPointer) dcd );
    
    XtRemoveCallback ( w, 
                      XmNhelpCallback, 
                      &DialogManager::helpCallback,
                      (XtPointer) dcd );
    
    delete dcd;
}

Widget DialogManager::getDialog()
{
    Widget newDialog = NULL;
     
    // If the permanent widget exists and is not in use,
    // just return it
     
     if ( _w && !XtIsManaged ( _w ) )
         return _w;
     
     // Get a widget from the derived class
 
     newDialog = createDialog ( theApplication->baseWidget() ) ;
 
     // If this is a temporary dialog, install callbacks to 
     // destroy it when the user pops it down.
 
     if ( _w )
     {
         XtAddCallback ( newDialog, 
                         XmNokCallback, 
                         &DialogManager::destroyTmpDialogCallback,
                         (XtPointer) this );
         
         XtAddCallback ( newDialog, 
                         XmNcancelCallback, 
                         &DialogManager::destroyTmpDialogCallback,
                         (XtPointer) this );
     }
     else                 // If this is the first dialog to be 
         _w = newDialog;  // created, save it to be used again.
     
     return newDialog;
}

void  DialogManager::destroyTmpDialogCallback ( Widget w, 
                                               XtPointer,
                                               XtPointer)
{
    XtDestroyWidget( w );
}



