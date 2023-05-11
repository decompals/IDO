/**********************************************************************************
  * chooseone.c: Test a list widget
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:      93-96
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
#include <Xm/List.h>

static void browse_callback();

main(argc, argv)
     int   argc;
     char *argv[];

{
  Widget    toplevel, list;
  Arg       wargs[10];
  int       n, i;
  XmString *xmstr;
  /*
   * Initialize the Intrinsics
   */  
  toplevel = XtInitialize (argv[0], "Chooseone", NULL, 0, 
                           &argc, argv);
  if(argc <= 1){
    XtCloseDisplay(XtDisplay(toplevel));
     exit(-1);
  }
 /*
  * Convert all command line arguments to an array of
  * type XmString, ignoring argv[0].
  */
  xmstr = (XmString *) XtMalloc(sizeof(XmString) * argc - 1);
  for(i = 1; i < argc; i++)
    xmstr[i - 1] = XmStringCreate(argv[i],
                                  XmSTRING_DEFAULT_CHARSET);
  /*
   * Create the list widget and register a browse callback.
   */
  n = 0;
  XtSetArg(wargs[n], XmNitems, xmstr); n++;  
  XtSetArg(wargs[n], XmNitemCount, argc - 1); n++;
  list = XtCreateManagedWidget("list", 
                               xmListWidgetClass, 
                               toplevel, wargs, n);
  XtAddCallback(list, XmNbrowseSelectionCallback,
                browse_callback, NULL);

  XtRealizeWidget (toplevel);
  XtMainLoop();
}

static void browse_callback(w, client_data, call_data)
   Widget     w;
   caddr_t    *client_data;
   XmListCallbackStruct *call_data;
{
  char *text;
  XmStringGetLtoR (call_data->item, XmSTRING_DEFAULT_CHARSET,
                   &text);
  printf("%s\n", text);
  XtCloseDisplay(XtDisplay(w));
  exit(0);
}
