/**********************************************************************************
  * draw.h: declarations for the draw program
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  266-279
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
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h> 
#include <X11/Xutil.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/Label.h>
#include "libXs.h"

#define MAXOBJECTS 1000

static unsigned char fg_bitmap[32] = {
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, /* 
solid foreground  */
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff  
};
#define fg_width  16
#define fg_height 16

static char *patterns[] = {  "foreground", 
                             "background",
                             "25_foreground",
                             "50_foreground",
                             "75_foreground",
                             "vertical",
                             "horizontal",
                             "slant_right",
                             "slant_left",
                           };

typedef struct {
  int  x1, y1, x2, y2;
  int  (*func) ();
  GC   gc;
} GBUFFER;

typedef struct {
  int          start_x, start_y, last_x, last_y;
  GC           xorgc;
  GC           gc;
  int          (*current_func)();
  int          foreground, background;
  GBUFFER      buffer[MAXOBJECTS];
  int          next_pos;
} graphics_data;

void  draw_line();
void  draw_circle();
void  draw_rectangle();
void  draw_filled_circle();
void  draw_filled_rectangle();

void  activate();
void  refresh();
void  set_stipple();
void  start_rubber_band();
void  track_rubber_band();
void  end_rubber_band();
void  set_fill_pattern();
