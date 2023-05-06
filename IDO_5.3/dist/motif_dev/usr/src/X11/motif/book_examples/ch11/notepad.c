/**********************************************************************************
  * notepad.c: Copy text to and from the clipboard
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  328-332
  *
  *
  *  Copyright 1989 by Prentice Hall
  *  All Rights Reserved
  *
  * This code is based on the OSF/Motif widget set and the X Window System
  *
  * Permission to use, copy, modify, and distribute this software for 
  * any purpose and without fee is hereby granted, provided that the above
  * copyright notice appear in all copies and that both the copyright notice
  * and this permission notice appear in supporting documentation.
  *
  * Prentice Hall and the author disclaim all warranties with regard to 
  * this software, including all implied warranties of merchantability and fitness.
  * In no event shall Prentice Hall or the author be liable for any special,
  * indirect or cosequential damages or any damages whatsoever resulting from 
  * loss of use, data or profits, whether in an action of contract, negligence 
  * or other tortious action, arising out of or in connection with the use 
  * or performance of this software.
  *
  * Open Software Foundation is a trademark of The Open Software Foundation, Inc.
  * OSF is a trademark of Open Software Foundation, Inc.
  * OSF/Motif is a trademark of Open Software Foundation, Inc.
  * Motif is a trademark of Open Software Foundation, Inc.
  * DEC is a registered trademark of Digital Equipment Corporation
  * HP is a registered trademark of the Hewlett Packard Company
  * DIGITAL is a registered trademark of Digital Equipment Corporation
  * X Window System is a trademark of the Massachusetts Institute of Technology
  **********************************************************************************/


#include <stdio.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h> 
#include <Xm/RowColumn.h>
#include <Xm/PanedW.h>
#include <Xm/Text.h> 
#include <Xm/PushB.h>
#include <Xm/CutPaste.h> 

extern void copy_to_clipboard (); 
extern void copy_from_clipboard (); 

main (argc, argv) 
   int    argc; 
   char  *argv[]; 
{ 
  Widget toplevel, base, commands, text, copy, get; 
  Arg    wargs[10]; 
  /* 
   * Initialize the Intrinsic and create a paned window 
   * widget as the base of the application. 
   */ 
  toplevel = XtInitialize (argv[0], "Notepad", NULL, 0,
                           &argc, argv); 
  base = XtCreateManagedWidget("base",
                               xmPanedWindowWidgetClass, 
                               toplevel, NULL, 0); 
  /* 
   * Make a row of command buttons. 
   */ 
  commands = XtCreateManagedWidget("commands",
                                   xmRowColumnWidgetClass, 
                                   base, NULL, 0); 
  copy = XtCreateManagedWidget("copy",
                               xmPushButtonWidgetClass, 
                               commands, wargs, 0); 
  get = XtCreateManagedWidget("retrieve",
                              xmPushButtonWidgetClass, 
                              commands, wargs, 0); 
  xs_create_quit_button(commands); 
 /* 
  * Create a multi-line text edit pane to hold notes 
  */ 
  XtSetArg(wargs[0], XmNeditMode, XmMULTI_LINE_EDIT); 
  text = XtCreateManagedWidget("text", xmTextWidgetClass, 
                               base, wargs, 1); 
  XtAddCallback (copy, XmNactivateCallback,
  copy_to_clipboard,  text); 
  XtAddCallback (get, XmNactivateCallback,
  copy_from_clipboard, text); 
  XtRealizeWidget(toplevel); 
  XtMainLoop(); 
} 

void copy_to_clipboard (w, text_w, call_data)
     Widget   w;
     Widget   text_w;
     XmAnyCallbackStruct *call_data;
{
   char    *selection = NULL;
   int     result;
   long    itemid, dataid;
   XButtonEvent *event = (XButtonEvent *) call_data->event;

 if ((selection = XmTextGetSelection (text_w)) != NULL) {
  /*
   * Begin the copy. If the clipboard is locked, 
   * keep trying.
   */
   while ((result = 
           XmClipboardStartCopy (XtDisplay(w), 
                                 XtWindow(text_w),
                                 "Notepad", 
                                 event->time,
                                 text_w, NULL, 
                                &itemid)) != ClipboardSuccess)
      ;
   /*
    * Copy the data.
    */
   while ((result =  
               XmClipboardCopy (XtDisplay(w),  
                                XtWindow(text_w), itemid,
                                XtRString, selection,
                                strlen(selection), 0,
                                &dataid)) != ClipboardSuccess)
      ;
   /*
    * End the transaction.
    */
   while ((result = 
              XmClipboardEndCopy(XtDisplay(w),
                                 XtWindow(text_w),
                                 itemid)) != ClipboardSuccess)
       ;
    }
}

void copy_from_clipboard (w, text_w, call_data) 
      Widget  w; 
      Widget  text_w; 
      caddr_t call_data;
 { 
    char buffer[BUFSIZ]; 
    int  length, id, result; 
    int  done = FALSE; 
    /* 
     * Retrieve the current contents of the clipboard. 
     */ 
    while (!done){ 
      result =  XmClipboardRetrieve (XtDisplay(w),
                                     XtWindow(text_w), 
                                     XtRString, buffer,
                                     BUFSIZ, &length, &id);
    /* 
     * Check the type. If the clipboard is locked, try
     * again. If data is retrieved successfully, append it 
     * to the text buffer. Otherwise abort. 
     */ 
    switch(result){ 
        case ClipboardSuccess: 
          buffer[length] = '\0'; 
          xs_insert_string(text_w, buffer); 
          done = TRUE; 
          break; 
        case   ClipboardTruncate: 
          XtWarning("Insufficient space for clipboard data"); 
          done = TRUE; 
        case   ClipboardNoData: 
          done = TRUE; 
          break; 
        case   ClipboardLocked: 
          break; 
     } 
   }
}
