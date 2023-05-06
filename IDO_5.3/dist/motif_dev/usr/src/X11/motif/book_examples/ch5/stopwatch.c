/**********************************************************************************
  * stopwatch.c: A digital stopwatch using workprocs.
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:      152-155
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
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <time.h>
#include "libXs.h"

Boolean update_time();
void    start_timing();
void    stop_timing();

long         start_time;
XtWorkProcId work_proc_id = NULL;

main(argc, argv)
   int      argc;
   char    *argv[];
{
  Widget   toplevel, panel, commands, start, stop, timer;

  toplevel = XtInitialize(argv[0], "Stopwatch", NULL, 0, 
                          &argc, argv);
  /*
   * Create a XmRowColumn widget to hold everything.
   */
  panel = XtCreateManagedWidget("panel", 
                                xmRowColumnWidgetClass,
                                toplevel, NULL, 0);
  /*
   * An XmLabel widget shows the current time.
   */ 
  timer = XtCreateManagedWidget("timer", 
                                xmLabelWidgetClass,
                                panel, NULL, 0);
  /*
   * Add start, stop, and quit buttons and register callbacks.
   * Pass the timer widget to all callbacks.
   */ 
  commands = XtCreateManagedWidget("commands", 
                                   xmRowColumnWidgetClass,
                                   panel, NULL, 0);
  start = XtCreateManagedWidget("start", 
                                 xmPushButtonWidgetClass,
                                 commands, NULL, 0);
  XtAddCallback(start,XmNactivateCallback,start_timing,timer);
  stop = XtCreateManagedWidget("stop", 
                               xmPushButtonWidgetClass,
                               commands, NULL, 0);
  XtAddCallback(stop, XmNactivateCallback,stop_timing,timer);
  xs_create_quit_button(commands);
 
  XtRealizeWidget(toplevel);
  XtMainLoop();
}

void start_timing(w, timer, call_data)
   Widget                w, timer;
   XmAnyCallbackStruct  *call_data;
{
  /* 
   * Get the initial time, and save it in a global.
   */
  time(&start_time); 
  /*
   * If a WorkProc has already been added, remove it.
   */
  if(work_proc_id)
    XtRemoveWorkProc(work_proc_id);
  /*
   * Register update_time() as a WorkProc.
   */
  work_proc_id = XtAddWorkProc(update_time, timer);
}

void stop_timing(w, timer, call_data)
   Widget                w, timer;
   XmAnyCallbackStruct  *call_data;
{
  if(work_proc_id)
    XtRemoveWorkProc(work_proc_id);
  work_proc_id = NULL; 
}
Boolean update_time(w)
      Widget   w;
{
  static long elapsed_time, last_time = -1;
  int minutes, seconds, current_time;
  /*
   * Retrieve the current time and calculate the elapsed time.
   */     
  time(&current_time);
  elapsed_time = current_time - start_time;
  /*
   * WorkProcs are irregularly called; don't update the
   * display if it's been less than a second since the last
   * time it was updated.
   */
  if(last_time == elapsed_time)
    return FALSE;
  /*
   * If one or more seconds has elapsed, remember this time,
   * and convert the elapsed time to minutes and seconds. 
   */
  last_time = elapsed_time;
  minutes = elapsed_time / 60;
  seconds = elapsed_time % 60;
  /*
   * Display the time as minutes and seconds.
   */
  xs_wprintf(w, "%02d : %02d", minutes, seconds);
  /*
   * Return FALSE so this WorkProc keeps getting called.
   */
  return FALSE;
}
