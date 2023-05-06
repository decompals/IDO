/**********************************************************************************
  * formedit.c: Simple example of multiple edit fields managed
  *             by an XmBulletinBoard widget. 
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:      81-83
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
#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/BulletinB.h>
#include "libXs.h"

void get_text();
char *buttons[] = {"button1", "button2", "button3"};
char *editors[] = {"field1", "field2", "field3"};

main(argc, argv)
   int   argc;
   char *argv[];
{
  Widget toplevel, bb, edit[3], button[3];
  int    i;
  toplevel = XtInitialize(argv[0], "Formedit",
                          NULL, 0 , &argc, argv);
  /*
   * Create the XmBulletinBoard widget that manages
   * the edit fields.
   */
  bb = XtCreateManagedWidget("board", 
                             xmBulletinBoardWidgetClass,
                             toplevel, NULL,0);
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
  XtManageChildren(edit, XtNumber(editors));
  XtManageChildren(button, XtNumber(buttons));

  XtRealizeWidget(toplevel);
  XtMainLoop();
}

void get_text(w, textwidget, call_value)
   Widget   w;
   Widget   textwidget;
   XmAnyCallbackStruct *call_value;
{
 printf("retrieving text: %s\n", XmTextGetString(textwidget));
}
