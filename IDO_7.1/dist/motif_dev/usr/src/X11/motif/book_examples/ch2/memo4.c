/**********************************************************************************
  * memo.c: Defining application actions and translations 
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:    Not in book
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
#include <Xm/Label.h>
#include "libXs.h"

static void quit();
static XtActionsRec actionsTable [] = {
  {"bye",   quit},
};

static char defaultTranslations[] =  "<Key>q:  bye(10)";

static void quit(w, event, params, num_params)
     Widget  w;
     XEvent *event;
     String *params;
     int     *num_params;
{
  if(*num_params == 1)
    sleep(atoi(params[0]));
  XtCloseDisplay(XtDisplay(w));
  exit(0);
}

main(argc, argv)
     int             argc;
     char           *argv[];
{
  Widget          toplevel, msg_widget;
  Arg             wargs[1];
  int             n;
  XmString        message;

  XtTranslations  trans_table;
  /*
   * Initialize Intrinsics
   */
  toplevel = XtInitialize(argv[0], "Memo", NULL, 0, 
			  &argc, argv);
  /*
   * Register the new actions, and compile 
   * translations table
   */
  XtAddActions(actionsTable, XtNumber(actionsTable));
  trans_table =
    XtParseTranslationTable(defaultTranslations); 
  /*
   * If a message is given on the command-line,
   * use it as the XtNstring argument for the widget
   */
  n = 0;
  if ((message = xs_concat_words(argc - 1, &argv[1])) != NULL){
    XtSetArg(wargs[n], XmNlabelString, message); n++;
  }
  /*
   * Create the message widget.
   */
  msg_widget = XtCreateManagedWidget("msg",  
				     xmLabelWidgetClass,
				     toplevel, wargs, n);
  /*
   * Merge the program-defined translations with 
   * existing translations.
   */
  XtAugmentTranslations(msg_widget, trans_table);
  /*
   * Realize all widgets and enter the event loop.
   */
  XtRealizeWidget(toplevel);
  XtMainLoop();
}





