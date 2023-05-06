/**********************************************************
 * A help callback.
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  119-121
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
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h> 
#include <Xm/Xm.h>
#include <Xm/MessageB.h>
#include "libXs.h"

static void xs_ok_callback();

void xs_help_callback(w, str, call_data) 
     Widget    w; 
     char     *str[];
     caddr_t   call_data;
{
  int        i, n;
  Widget     dialog;
  Widget     label;
  XmString   xmstr;
  Arg        wargs[5];
  /*
   * Create the message dialog to display the help.
   */
   n = 0;
   XtSetArg(wargs[n], XmNautoUnmanage, FALSE); n++;
   dialog = XmCreateMessageDialog(w, "Help", wargs, n);
   /*
    * We won't be using the cancel widget. Unmanage it.
    */
   XtUnmanageChild(XmMessageBoxGetChild (dialog,
                   XmDIALOG_CANCEL_BUTTON));
   /*
    * Retrieve the label widget and make the 
    * text left justified
    */
   label = XmMessageBoxGetChild (dialog,
                                 XmDIALOG_MESSAGE_LABEL);
   n = 0;
   XtSetArg(wargs[n],XmNalignment,XmALIGNMENT_BEGINNING);n++;
   XtSetValues(label, wargs, n);
   /*
    * Add an OK callback to pop down the dialog.
    */
   XtAddCallback(dialog, XmNokCallback, 
                 xs_ok_callback, NULL);
  /*
   * Count the text up to the first NULL string.
   */
  for(i=0; str[i][0] != '\0'; i++)
    ;
  /*
   * Convert the string array to an XmString array and 
   * set it as the label text.
   */
  xmstr  = xs_str_array_to_xmstr(str, i);
  n = 0;
  XtSetArg(wargs[n],  XmNmessageString, xmstr);n++;
  XtSetValues(dialog, wargs, n);
  /*
   * If the next entry in the help string array is also NULL,
   * then this is the last message. Unmanage the help button.
   */
  if(str[++i][0] == '\0')
    XtUnmanageChild(XmMessageBoxGetChild (dialog,
                    XmDIALOG_HELP_BUTTON));
  /*
   * Otherwise, add a help callback function with the address of 
   * the next entry  in the help string as client_data.
   */
  else{
    XtAddCallback(dialog, XmNhelpCallback, 
                  xs_help_callback, &str[i]);
  }
  /*
   * Display the dialog.
   */
  XtManageChild(dialog);
}

static void xs_ok_callback(w, client_data, call_data) 
   Widget               w; 
   caddr_t              client_data;
   XmAnyCallbackStruct *call_data; 
{
   XtUnmanageChild(w);
   XtDestroyWidget(w);   
}
