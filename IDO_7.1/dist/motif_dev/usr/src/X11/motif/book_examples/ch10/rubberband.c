/**********************************************************************************
  * rubberband.c: rubberband line example
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  259-263
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
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include "libXs.h"

typedef struct {
    int start_x, start_y, last_x, last_y;
    GC  gc;
  } rubber_band_data;

void start_rubber_band();
void end_rubber_band();
void track_rubber_band();


main(argc, argv)
 int   argc;
 char *argv[];
{
  Widget           toplevel, canvas;
  rubber_band_data data;
   
  toplevel = XtInitialize(argv[0], "Rubberband", NULL, 0, 
                          &argc, argv);
  /*
   * Create a drawing surface, and add event handlers for
   * ButtonPress, ButtonRelease and MotionNotify events.
   */
  canvas = XtCreateManagedWidget("canvas", 
                                 xmDrawingAreaWidgetClass, 
                                 toplevel, NULL, 0);
  XtAddEventHandler(canvas, ButtonPressMask, FALSE,
                    start_rubber_band, &data);
  XtAddEventHandler(canvas, ButtonMotionMask, FALSE,
                    track_rubber_band, &data);
  XtAddEventHandler(canvas, ButtonReleaseMask,
                    FALSE, end_rubber_band, &data);
  XtRealizeWidget(toplevel);
  /*
   * Establish a passive grab, for any button press.
   * Force the sprite to stay within the canvas window, and
   * change the sprite to a cross_hair.
   */
   XGrabButton(XtDisplay(canvas), AnyButton, AnyModifier, 
               XtWindow(canvas), TRUE, 
               ButtonPressMask | ButtonMotionMask | 
               ButtonReleaseMask,
               GrabModeAsync, GrabModeAsync,
               XtWindow(canvas), 
               XCreateFontCursor(XtDisplay(canvas),
                                 XC_crosshair));
  /*
   * Create the GC used by the rubber banding functions.
   */
  data.gc = xs_create_xor_gc(canvas); 
  XtMainLoop();
}

void start_rubber_band(w, data, event)
    Widget             w;
    rubber_band_data   *data;
    XEvent             *event;
{
  data->last_x  =  data->start_x = event->xbutton.x;
  data->last_y  =  data->start_y = event->xbutton.y;
   XDrawLine(XtDisplay(w), XtWindow(w), 
            data->gc, data->start_x, 
            data->start_y, data->last_x, data->last_y);
}

void track_rubber_band(w, data, event)
    Widget             w;
    rubber_band_data  *data;
    XEvent            *event;
{
  /*
   * Draw once to clear the previous line.
   */
  XDrawLine(XtDisplay(w), XtWindow(w), data->gc, 
            data->start_x,data->start_y, 
            data->last_x, data->last_y);
  /*
   * Update the endpoints.
   */
  data->last_x  =  event->xbutton.x;
  data->last_y  =  event->xbutton.y;
  /*
   * Draw the new line.
   */
  XDrawLine(XtDisplay(w), XtWindow(w), data->gc, 
            data->start_x, data->start_y, 
            data->last_x, data->last_y);
}

void end_rubber_band(w, data, event)
   Widget            w;
   rubber_band_data *data;
   XEvent           *event;
{
 /*
  * Clear the current line and update the endpoint info.
  */
  XDrawLine(XtDisplay(w), XtWindow(w), data->gc, 
            data->start_x, data->start_y, 
            data->last_x, data->last_y);
  data->last_x  =  event->xbutton.x;
  data->last_y  =  event->xbutton.y;
}
