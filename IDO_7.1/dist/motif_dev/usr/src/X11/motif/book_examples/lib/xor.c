/**************************************************************************
 * xs_create_xor_gc():  utility routine  
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  ??
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

GC xs_create_xor_gc(w)
   Widget            w;
{
  XGCValues values;
  GC        gc;
  Arg       wargs[10];
  /*
   * Get the colors used by the widget.
   */
  XtSetArg(wargs[0], XtNforeground, &values.foreground);
  XtSetArg(wargs[1], XtNbackground, &values.background);
  XtGetValues(w, wargs,2);
  /*
   * Set the fg to the XOR of the fg and bg, so if it is
   * XOR'ed with bg, the result will be fg and vice-versa.
   * This effectively achieves inverse video for the line.
   */
  values.foreground = values.foreground ^ values.background;
  /*
   * Set the rubber band gc to use XOR mode and draw 
   * a dashed line.
   */
  values.line_style = LineOnOffDash;
  values.function   = GXxor;
  gc = XtGetGC(w, GCForeground | GCBackground | 
               GCFunction | GCLineStyle, &values);
  return gc;
}
