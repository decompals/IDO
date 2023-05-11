/**********************************************************************************
  *  memo.c: Selection Version
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  317-321
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

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <X11/Xatom.h>
#include "libXs.h"

void       grab_selection();
Boolean    convert_selection();
void       lose_selection();
Atom       COMPOUND_STRING;

main(argc, argv)
   int         argc;
   char       *argv[];
{
  Widget       toplevel, msg_widget, base;
  Arg          wargs[1];
  int          n;
  XmString     xmstr;
  toplevel = XtInitialize(argv[0],"Memo", NULL, 0, 
                          &argc, argv);
  COMPOUND_STRING = XInternAtom(XtDisplay(toplevel), 
                                "Compound String", FALSE);
  /*
   *  Create a manager for the quit button and message window.
   */
  base = XtCreateManagedWidget("base", xmRowColumnWidgetClass,
                                  toplevel, NULL, 0);
  xs_create_quit_button(base);
  /*
   * Get the contents of the command line and display it in
   * the message window.
   */
  n = 0;
  if((xmstr = xs_concat_words(argc - 1, &argv[1])) != NULL)
    XtSetArg(wargs[n], XmNlabelString, xmstr); n++;

  msg_widget = XtCreateManagedWidget("message", 
                                     xmLabelWidgetClass,
                                     base, wargs, n);
  XtAddEventHandler(msg_widget, ButtonPressMask,
                    FALSE, grab_selection, NULL);
  /*
   * Realize all widgets and enter the event loop.
   */
  XtRealizeWidget(toplevel);
  XtMainLoop();
}

void grab_selection(w, client_data, event)
     Widget    w;
     caddr_t   client_data;
     XEvent   *event;
{
  /*
   * Claim ownership of the PRIMARY selection.
   */
  if(XtOwnSelection(w, XA_PRIMARY, 
                    event->xbutton.time, 
                    convert_selection,  /* handle requests */
                    lose_selection,     /* Give up selection*/
                    NULL)){
    xs_invert_widget(w);
    XtSetSensitive(w, FALSE);
  }
}

static Boolean convert_selection(w, selection, target,
                                 type, value, length, format)
    Widget         w;
    Atom          *selection, *target, *type;
    caddr_t       *value;
    unsigned long *length;
    int           *format;
{
   Arg wargs[10];

  if (*target == COMPOUND_STRING) {
    XmString xmstr;
    XtSetArg(wargs[0], XmNlabelString, &xmstr);
    XtGetValues(w, wargs, 1);
    *type   = COMPOUND_STRING;
    *value  = xmstr;
    *length = XmStringLength(xmstr);
    *format = 8;
    return TRUE;
  }
 else if (*target == XA_STRING) {
    char     *str;
    XmString  xmstr;

    XtSetArg(wargs[0], XmNlabelString, &xmstr);
    XtGetValues(w, wargs, 1);

    *type   = XA_STRING;
    *value  = xs_get_string_from_xmstring (xmstr);
    *length = strlen(*value);
    *format = 8;
    return TRUE;
  }
   else
     return FALSE;
}


static void lose_selection(w, selection)
  Widget   w;
  Atom    *selection;
{
  xs_invert_widget(w);
  XtSetSensitive(w, TRUE);
}


