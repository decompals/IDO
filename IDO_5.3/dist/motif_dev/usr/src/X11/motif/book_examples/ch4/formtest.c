/**********************************************************************************
  * formtest.c: Test the constraints of the XmForm Widget
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:      88-92
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
#include <Xm/Form.h>
#include <Xm/PushB.h>

char * buttons[] = {"button1", "button2", "button3"};

main(argc, argv)
     int   argc;
     char *argv[];
{
  Widget toplevel, form, wbutton[5];
  int    i, n;
  Arg wargs[10];
  toplevel = XtInitialize(argv[0], "Formtest", NULL, 0, 
                          &argc, argv);

  /*
   * Create an XmForm manager widget
   */
  form = XtCreateManagedWidget("form", xmFormWidgetClass, 
                               toplevel, NULL,0);
  /*
   * Add three XmPushButton widgets to the Form Widget.
   */
  for(i=0;i< XtNumber(buttons); i++)
    wbutton[i] = XtCreateWidget(buttons[i],
                                xmPushButtonWidgetClass, 
                                form, NULL,0);
  XtManageChildren(wbutton, XtNumber(buttons));
  /*
   * Set constraint resources for each button, setting up
   * a shape like this:
   *          button one
   *          button two
   *          button three
   */
  n = 0;
  XtSetArg(wargs[n], XmNtopAttachment,   XmATTACH_FORM); n++;
  XtSetArg(wargs[n], XmNleftAttachment,  XmATTACH_FORM); n++;
  XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM); n++;
#ifndef JUST_LIKE_BOOK
  /*
   * The book code assumed no application defaults file when running
   * this * example.  On line, we provide one because the following
   * formtest example does need one.  Therefore we set XmNbottomAttachment
   * for buttons 1 and 2 to what it would default to in the absence
   * of the defaults file.
   */
  XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_NONE); n++;
#endif
  XtSetValues(wbutton[0], wargs, n);
  n = 0;
  XtSetArg(wargs[n], XmNtopAttachment,   XmATTACH_WIDGET);n++;
  XtSetArg(wargs[n], XmNtopWidget,       wbutton[0]);     n++;
  XtSetArg(wargs[n], XmNleftAttachment,  XmATTACH_FORM);  n++;
  XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM);  n++;
#ifndef JUST_LIKE_BOOK
  XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_NONE); n++;
#endif
  XtSetValues(wbutton[1], wargs, n);
  n = 0;
  XtSetArg(wargs[n], XmNtopAttachment,   XmATTACH_WIDGET);n++;
  XtSetArg(wargs[n], XmNtopWidget,       wbutton[1]);     n++;
  XtSetArg(wargs[n], XmNleftAttachment,  XmATTACH_FORM);  n++;
  XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_FORM);  n++;
  XtSetArg(wargs[n], XmNbottomAttachment,XmATTACH_FORM);  n++;
  XtSetValues(wbutton[2], wargs, n);

  XtRealizeWidget(toplevel);
  XtMainLoop();
}
