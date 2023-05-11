/**********************************************************************************
  * xbc.c: An X interface to bc
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:   149-151
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
#include <time.h>

void update_time();

main(argc, argv)
   int             argc;
   char           *argv[];
{
   Widget          toplevel, clock;
   /*
    * Create the widgets.
    */
   toplevel = XtInitialize(argv[0], "Clock", NULL, 0, 
                           &argc, argv);
   clock = XtCreateManagedWidget("face", 
                                 xmLabelWidgetClass,
                                 toplevel, NULL, 0);
   /*
    * Get the initial time.
    */
   update_time(clock, NULL);
   XtRealizeWidget(toplevel);
   XtMainLoop();
}
void update_time(w, id)
    Widget          w;
    XtIntervalId    id;
{
    long   tloc, rounded_tloc, next_minute;
   /*
     * Ask Unix for the time.
     */
    time(&tloc);
    /*
     * Convert the time to a string and display it,
     * after rounding it down to the last minute.
     */
    rounded_tloc = tloc / 60 * 60;
    xs_wprintf(w, "%s", ctime(&rounded_tloc));
    /*
     * Adjust the time to reflect the time till 
     * the next round minute.
     */
    next_minute = (60 - tloc % 60) * 1000;
    /*
     * The Intrinsics removes timeouts when they occur,
     * so put ourselves back. 
     */
    XtAddTimeOut(next_minute, update_time, w);
}
