/**********************************************************************************
  * SquareDial.c: A subclass of the Dial widget class
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  364-369
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
#include <X11/IntrinsicP.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/CoreP.h>
#include "DialP.h"
#include "Dial.h"
#include "SquareDialP.h"
#include "SquareDial.h"
#define  MIN(a,b)    (((a) < (b)) ? (a) :  (b))
static void Resize();
static void Initialize();

XsSquareDialClassRec  XssquareDialClassRec = {
/* CoreClassPart */
  {
   (WidgetClass) &XsdialClassRec,  /* superclass            */
   "SquareDial",                   /* class_name            */
   sizeof(XsSquareDialRec),        /* widget_size           */
   NULL,                           /* class_initialize      */
   NULL,                           /* class_part_initialize */
   FALSE,                          /* class_inited          */
   (XtWidgetProc) Initialize,      /* initialize            */
   NULL,                           /* initialize_hook       */
   XtInheritRealize,               /* realize               */
   NULL,                           /* actions               */
   0,                              /* num_actions           */
   NULL,                           /* resources             */
   0,                              /* num_resources         */
   NULLQUARK,                      /* xrm_class             */
   TRUE,                           /* compress_motion       */
   TRUE,                           /* compress_exposure     */
   TRUE,                           /* compress_enterleave   */
   TRUE,                           /* visible_interest      */
   NULL,                           /* destroy               */
   (XtWidgetProc) Resize,          /* resize                */
   XtInheritExpose,                /* expose                */
   NULL,                           /* set_values            */
   NULL,                           /* set_values_hook       */
   XtInheritSetValuesAlmost,       /* set_values_almost     */
   NULL,                           /* get_values_hook       */
   XtInheritAcceptFocus,           /* accept_focus          */
   XtVersion,                      /* version               */
   NULL,                           /* callback private      */
   XtInheritTranslations,          /* tm_table              */
   NULL,                           /* query_geometry        */
   NULL,                           /* display_accelerator   */
   NULL                            /* extension             */
   },
      /* Dial class fields */
  {
   0,                              /* ignore                */
   },
      /* Square Dial class fields */
  {
   0,                              /* ignore                */
   }
};

WidgetClass XssquareDialWidgetClass = 
         (WidgetClass) &XssquareDialClassRec;

static void Initialize(request, new)
  XsSquareDialWidget    request, new;
{
  Resize(new);
}

static void Resize(w)
  XsSquareDialWidget    w;
{
  int      marks_per_side, h_increment, v_increment, i;
  XPoint  *ptr;
  /*
   * Get the address of the segment array.
   */
  ptr = w->dial.segments;
  /*
   * Calculate the center of the window.
   */
  w->dial.center_x = w->core.width / 2; 
  w->dial.center_y = w->core.height / 2; 

  w->dial.outer_diam = MIN(w->core.width, w->core.height) / 2;
  w->dial.inner_diam = w->dial.outer_diam - w->dial.marker_length; 
  /* 
   * Position the marks up the left side, across the top,   
   * and down the right side of the window.   
   */
   marks_per_side  = w->dial.markers / 3;
   w->dial.markers = marks_per_side * 3;
   h_increment = w->core.width / (marks_per_side + 1);
   v_increment = w->core.height / (marks_per_side + 1);
  /* 
   * Do the left side.  
  */
  for(i=0;i<marks_per_side;i++){
    ptr->x   = 0; 
    ptr++->y =  w->core.height - i * v_increment - v_increment;
    ptr->x   = w->dial.marker_length; 
    ptr++->y = w->core.height - i * v_increment - v_increment;
   }
   /* 
    * Do the top. 
    */
   for(i=0;i<marks_per_side;i++){
    ptr->x    = h_increment + i * h_increment;
    ptr++->y  = 0; 
    ptr->x    = h_increment + i * h_increment; 
    ptr++->y  = w->dial.marker_length;     
  }
   /*
    * Do the right side. 
    */
   for(i=0;i<marks_per_side;i++){
    ptr->x   = w->core.width - w->dial.marker_length; 
    ptr++->y = w->core.height - i * v_increment - v_increment; 
    ptr->x   = w->core.width; 
    ptr++->y = w->core.height - i * v_increment - v_increment; 
  }
} 
