/**********************************************************************************
  * helptest.c: Test a popup Dialog widget with a help button
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:     117-128
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
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/BulletinB.h>
#include <Xm/Text.h>

#include "libXs.h"

void   show_dialog();
void   done_callback();
Widget create_dialog();
void   get_text();

main(argc, argv)
     int   argc;
     char *argv[];
{
  Widget toplevel, button, dialog;
  /*
   * Initialize the Intrinsics and create a PushButton widget.
   */  
  toplevel = XtInitialize (argv[0], "Helptest", NULL, 0, 
                           &argc, argv);
  button = XtCreateManagedWidget("button", 
                                 xmPushButtonWidgetClass, 
                                 toplevel, NULL, 0);
  /*
   * Create a popup dialog and a register a callback
   * function to popup it up when the button is activated.
   */
  dialog = create_dialog(button);
  XtAddCallback(button, XmNactivateCallback, 
                show_dialog, dialog);

  XtRealizeWidget (toplevel);
  XtMainLoop();
}

static void show_dialog(w, dialog, call_data)
   Widget     w;
   Widget     dialog;
   XmAnyCallbackStruct *call_data;
{
  XtManageChild(dialog);
}

char *buttons[] = {"button1", "button2", "button3"};
char *editors[] = {"field1", "field2", "field3"};

char *help_str[] = {
  "Fill in the text fields and press the buttons to ",
  " the right when done",
  "",
  "The first text field should contain a persons name",
  "The second field should contain an address",
  "The third should contain a phone number",
  "Edit any field and then press the button to the right",
  "to indicate that the information is completed",
  "",
  "Sorry, more detailed help is unavailable.",
  "",""};

Widget create_dialog(parent, name)
     Widget parent;
{
  Widget bb, edit[3], button[3], done_button, help_button;
  Arg    wargs[10];
  int    i,  n = 0;
  XtSetArg(wargs[n], XmNautoUnmanage, FALSE); n++;
  bb = XmCreateBulletinBoardDialog(parent, "board", wargs, n);
  /*
   * Create three single line XmEdit widgets
   * and associate a button with each text widget.
   * Assign an XmNactivateCallback callback to each button.
   */
  for(i=0; i < XtNumber(editors); i++)
     edit[i] = XtCreateWidget(editors[i], 
                              xmTextWidgetClass,
                              bb, NULL, 0);
  for(i=0; i < XtNumber(buttons); i++){
     button[i] = XtCreateWidget(buttons[i],
                                xmPushButtonWidgetClass,
                                bb, NULL, 0);
     XtAddCallback(button[i], XmNactivateCallback, 
                   get_text, edit[i]);
  }
  /*
   * Add a button to let the user pop down the widget.
   */
  done_button = XtCreateManagedWidget("done",
                                      xmPushButtonWidgetClass,
                                      bb, NULL, 0);
  XtAddCallback(done_button, XmNactivateCallback,
                done_callback, bb);
  /*
   * Add a button to let the user ask for help.
   */
  help_button = XtCreateManagedWidget("help",
                                      xmPushButtonWidgetClass,
                                      bb, NULL, 0);
  XtAddCallback(help_button, XmNactivateCallback,
                xs_help_callback, help_str);
  XtManageChildren(edit, XtNumber(editors));
  XtManageChildren(button, XtNumber(buttons));
  return bb;
}

static void done_callback(w, dialog, call_data)
   Widget     w;
   Widget     dialog;
   XmAnyCallbackStruct *call_data;
{
  XtUnmanageChild(dialog);
#ifdef JUST_LIKE_BOOK
  /*
   * Oops, version-itis got me here. If the widget is re-created everytime
   * it is popped up, in show_dialog() this would be ok. As it is, popping
   * up the widget a second time after it has been destroyed will be trouble.
   */
  XtDestroyWidget(dialog);
#endif
}
void get_text(w, textwidget, call_value)
   Widget   w;
   Widget   textwidget;
   XmAnyCallbackStruct *call_value;
{
 printf("retrieving text: %s\n", XmTextGetString(textwidget));
}
