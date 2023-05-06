/**********************************************************************************
  * rctest.c: An example using the XmRowColumn widget
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:     85-86
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
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

char * buttons[] = {"button1", "button2", "button3", 
                    "button4", "button5", "button6"};
main(argc, argv)
  int    argc;
  char  *argv[];
{
  Widget toplevel, rowcol;
  int    i;
  toplevel = XtInitialize(argv[0], "Rctest", NULL, 0, 
                          &argc, argv);
  /*
   * Create an XmRowColumn widget.
   */
  rowcol = XtCreateManagedWidget("rowcol",
                                 xmRowColumnWidgetClass,
                                 toplevel, NULL, 0);
  /*
   * Create six children of the XmRowColumn widget.
   */ 
  for(i=0;i< XtNumber(buttons); i++)  
   XtCreateManagedWidget(buttons[i], xmPushButtonWidgetClass,
                         rowcol, NULL, 0);
  XtRealizeWidget(toplevel);
  XtMainLoop();
}
