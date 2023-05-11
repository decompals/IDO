/**********************************************************************************
  * fileview.h: declarations for fileview program
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  220-230
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
#define ABS(a)       (((a) >= 0) ? (a) : -(a))
#define MAX(a,b)     ((a > b) ? a : b)
#define MARGIN         5


typedef struct {
  char         *chars[MAXLINES];   /* Lines of text         */
  int           length[MAXLINES];  /* Length of each line   */
  int           rbearing[MAXLINES];/* right bearing of line */
  int           descent;           /* descent below baseline*/
  XFontStruct  *font;              /* The font struct       */
  GC            gc;                /* A read/write GC       */
  Widget        scrollbar;
  Widget        canvas;
  Dimension     canvas_height;     /* canvas dimensions     */
  Dimension     canvas_width;
  int           fontheight;        /* descent + ascent      */
  int           nitems;            /* number of text lines  */
  int           top;               /* line at top of window */
 } text_data, *text_data_ptr;

void       handle_exposures();
void       scroll_bar_moved ();
void       resize ();

static XtResource resources[] = {
 {XtNfont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
    XtOffset(text_data_ptr, font), XtRString, "Fixed"      },
};
