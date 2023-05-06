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
  *                 Example described on pages:     ??
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
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include "libXs.h"

create_mouse_tracker(parent, target)
    Widget   parent, target;
{
    extern void  clear_tracker();
    extern void  track_mouse_position();
    Widget       tracker;
    /*
     * Create the tracker widget and register event 
     * handlers for the target widget. 
     */
    tracker = XtCreateManagedWidget("mousetracker", 
                                    xmLabelWidgetClass,
                                    parent, NULL, 0);
    XtAddEventHandler(target, LeaveWindowMask, FALSE,
                      clear_tracker, tracker);
    XtAddEventHandler(target, PointerMotionMask, FALSE,
                      track_mouse_position, tracker);
}

void track_mouse_position(w, tracker, event)
    Widget          w;
    Widget          tracker;
    XEvent         *event;
{
   /*
    * Extract the position of the sprite from the event
    * and display it in the tracker widget. 
    */
    xs_wprintf(tracker, "X: %04d, Y: %04d", 
               event->xmotion.x, event->xmotion.y);
}

void clear_tracker(w, tracker, event)
    Widget      w;
    Widget      tracker;
    XEvent     *event;
{
  /*  
   * Display an empty string in the tracker widget. 
   */
#ifdef JUST_LIKE_BOOK
  xs_wprintf(tracker, "");
#else
  /*
   * If you set the label to null, it cannot later resize.  This is a motif
   * bug.  The work-around is to set the label to just a space.
   */
  xs_wprintf(tracker, " ");
#endif
}
