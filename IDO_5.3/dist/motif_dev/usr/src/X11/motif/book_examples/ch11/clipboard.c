/**********************************************************************************
  * clipboard.c: A simple clipboard using X selections
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  321-325
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
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <X11/Xatom.h>
#include "libXs.h"

void      grab_selection();
void      request_selection();
Boolean   convert_selection();
void      lose_selection();
void      show_selection();
void      toggle_type();
Atom      COMPOUND_STRING;
Atom      current_type = XA_STRING;

main(argc, argv)
    int             argc;
    char           *argv[];
{
  Widget   toplevel, selection, request, row_col, toggle;

  toplevel = XtInitialize(argv[0], "Clipboard", NULL, 
                          0, &argc, argv);
  COMPOUND_STRING = XInternAtom(XtDisplay(toplevel), 
                                "Compound String", FALSE);

  row_col = XtCreateManagedWidget("commands",
                                  xmRowColumnWidgetClass,
                                  toplevel, NULL, 0);
  xs_create_quit_button(row_col); 
  /*
   * Create a button used to request the selection and
   * a text widget to display it.
   */
  request = XtCreateManagedWidget("getselection", 
                                  xmPushButtonWidgetClass,
                                  row_col, NULL, 0); 
  toggle = XtCreateManagedWidget("type", 
                                 xmToggleButtonWidgetClass,
                                 row_col, NULL, 0); 
  xs_wprintf(toggle, "Type: Character String");

  selection = XtCreateManagedWidget("currentselection", 
                                    xmLabelWidgetClass,
                                    row_col, NULL, 0);

  XtAddCallback(toggle, XmNvalueChangedCallback, 
                toggle_type, NULL);
#ifdef JUST_LIKE_BOOK
  XtAddEventHandler(request, ButtonPress, FALSE,
                    request_selection, selection);
  XtAddEventHandler(selection, ButtonPress, FALSE,
                    grab_selection, NULL);
#else
  XtAddCallback(request, XmNactivateCallback,
                    request_selection, selection);
  XtAddEventHandler(selection, ButtonPressMask, FALSE,
                    grab_selection, NULL);
#endif

  
  XtRealizeWidget(toplevel);
  XtMainLoop();
}

void request_selection(w, client_data, call_data)
     Widget          w;
     caddr_t         client_data;
     XmAnyCallbackStruct *call_data;

{
  XtGetSelectionValue(w, XA_PRIMARY, current_type,
                      show_selection, client_data, 
                      call_data->event->xbutton.time);
}

static void show_selection(w, client_data, selection, type,
                           value, length, format)
    Widget         w;
    caddr_t        client_data;
    Atom          *selection, *type;
    caddr_t        value;
    unsigned long *length;
    int           *format;
{
  Arg wargs[2];
  if (*type == COMPOUND_STRING){
     XtSetArg(wargs[0], XmNlabelString, value);
     XtSetValues(client_data, wargs, 1);
  }
  else if (*type == XA_STRING){
     xs_wprintf(client_data, "%s", value);
  }
}

void toggle_type (w, client_data, call_data)
     Widget          w;
     void           *client_data;
     XmToggleButtonCallbackStruct *call_data;
{
   if(call_data->set){
     current_type = COMPOUND_STRING;
     xs_wprintf(w, "Type: Compound String");
   }
   else{
    current_type = XA_STRING;
    xs_wprintf(w, "Type: Character String");
  }
}

void grab_selection(w, client_data, event)
     Widget    w;
     void     *client_data;
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
