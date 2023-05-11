/**********************************************************************************
  * fileview2.h: declarations for fileview program
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  241-235
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


#include <stdio.h>
#include <X11/Intrinsic.h> 
#include <X11/StringDefs.h>
#include <X11/Xutil.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include "libXs.h"

#define MAXLINESIZE  300
#define MAXLINES    2000
#define MIN(a,b)     (((a) < (b)) ? (a) : (b))
#define MAX(a,b)     (((a) > (b)) ? (a) : (b))
#define ABS(a) (((a) >= 0) ? (a) : -(a))
#define MARGIN         5

typedef struct {
  XmString      chars[MAXLINES];
  Dimension     width[MAXLINES];
  int           descent;
  XFontStruct   *font;
  XmFontList    fontlist;
  GC            gc;
  Widget        scrollbar;
  Widget        canvas;
  Dimension     canvas_height;
  Dimension     canvas_width;
  int           fontheight;
  int           nitems;
  int           top;
  } text_data, *text_data_ptr;

void       handle_exposures();
void       scroll_bar_moved ();
void       resize ();

static XtResource resources[] = {
  {XtNfont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
     XtOffset(text_data_ptr, font), XtRString, "Fixed"   }, 
};


