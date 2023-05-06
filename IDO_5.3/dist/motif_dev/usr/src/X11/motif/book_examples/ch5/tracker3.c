/**********************************************************************************
  * tracker3.c: 
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:    145-149
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

typedef struct {
    Widget        tracker;
    Widget        target;
    XtIntervalId  id;
    int           delay;
} track_data, *track_data_ptr; 

#define DELAY 1000

create_mouse_tracker(parent, target)
     Widget          parent, target;
{
  extern void       enter_window_handler();
  static track_data data;

  data.delay = DELAY;
  /*
   * Store the target and tracker widgets in the data.
   */
  data.target  = target;
  data.tracker = XtCreateManagedWidget("mousetracker", 
                                        xmLabelWidgetClass,
                                        parent, NULL, 0);
  /*
   * Start with a single event handler.
   */  
  XtAddEventHandler(data.target, EnterWindowMask, FALSE,
                    enter_window_handler, &data);
}

static void enter_window_handler(w, data, event)
    Widget          w;
    track_data     *data;
    XEvent         *event;
{
   extern void  start_tracking();
   extern void  disable_alarm();
   /*
    * When the sprite enters the window, install
    * a timeout callback, and start the count-down.
    */
   XtAddEventHandler(data->target, LeaveWindowMask, FALSE,
                     disable_alarm, data);
   data->id = XtAddTimeOut(data->delay, start_tracking, data);
}

static void disable_alarm(w, data, event)
   Widget          w;
   track_data     *data;
   XEvent         *event;
{
   /*
    * Remove the timeout callback and then remove
    * ourself as an event handler.
    */
   XtRemoveTimeOut(data->id);
   XtRemoveEventHandler(data->target, LeaveWindowMask, FALSE,
                        disable_alarm, data);
}

static void start_tracking(data, id)
    track_data     *data;
    XtIntervalId    id;
{
    extern void  disable_alarm();
    extern void  leave_window_handler();
    extern void  track_mouse_position();
    /*
     * If this function was called, the alarm must have
     * gone off, so remove the disable_alarm event handler.
     */
    XtRemoveEventHandler(data->target, LeaveWindowMask, 
                         FALSE, disable_alarm, data);
    /*
     * Now add event handlers to track the sprite motion
     * and clear the tracker when we leave the target window.
     */
    XtAddEventHandler(data->target, PointerMotionMask, 
                      FALSE, track_mouse_position, data);
    XtAddEventHandler(data->target, LeaveWindowMask, 
                      FALSE, leave_window_handler, data);
}

static void track_mouse_position(w, data, event)
    Widget          w;
    track_data     *data;
    XEvent         *event;
{
  /*
   * Extract the position of the sprite from the event
   * and display it in the tracker widget. 
   */
  xs_wprintf(data->tracker, "X: %04d, Y: %04d",
             event->xmotion.x, event->xmotion.y);
}

static void leave_window_handler(w, data, event)
   Widget          w;
   track_data     *data;
   XEvent         *event;
{
 extern void  track_mouse_position();
 /*
  * Clear the tracker widget display.
  */
#ifdef JUST_LIKE_BOOK
  xs_wprintf(data->tracker, "");
#else
  /*
   * If you set the label to null, it cannot later resize.  This is a motif
   * bug.  The work-around is to set the label to just a space.
   */
  xs_wprintf(data->tracker, " ");
#endif
 /*
  * Remove the dynamically installed event handlers.
  */
  XtRemoveEventHandler(data->target, PointerMotionMask, FALSE,
                       track_mouse_position, data);
  XtRemoveEventHandler(data->target, LeaveWindowMask, FALSE,
                       leave_window_handler, data);
}

