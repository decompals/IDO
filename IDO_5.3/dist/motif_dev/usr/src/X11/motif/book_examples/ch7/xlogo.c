/**********************************************************************************
 * xlogo.c: Display the X logo
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  191-196
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
#include <Xm/DrawingA.h>
#include <X11/Xutil.h>
#include "xlogo64"

void     redisplay();
void     resize();
extern Pixmap create_logo();

typedef struct {
   Pixmap     pix;
   GC         gc;
   Dimension  width, height;
} pixmap_data;

main(argc, argv)
  int   argc;
  char *argv[];
{
  Widget       toplevel, canvas;
  Arg          wargs[10];
  int          n;
  XGCValues    values;
  pixmap_data  data;
    
  toplevel = XtInitialize(argv[0], "Xlogo", NULL, 0, 
                          &argc, argv);
  /*
   * Create a widget in which to display the logo.
   */
  canvas = XtCreateManagedWidget("canvas", 
                                 xmDrawingAreaWidgetClass, 
                                 toplevel, NULL, 0);
 /*
  * Use the foreground and background colors
  * of the canvas to create a graphics context.
  */
  n = 0;
  XtSetArg(wargs[n], XtNforeground, &values.foreground);n++;
  XtSetArg(wargs[n], XtNbackground, &values.background);n++;
  XtGetValues(canvas, wargs, n);
  data.gc = XtGetGC(canvas, GCForeground | GCBackground, &values);
  /*
   * Create the pixmap conatinign the X logo. Store the 
   * pixmap, as well as the size of the pixmap in the struct.
   */
  data.width = xlogo64_width;
  data.height = xlogo64_height;
  data.pix = create_logo(canvas, data.gc, xlogo64_bits, 
                         xlogo64_width, xlogo64_height );

  XtAddCallback(canvas, XmNexposeCallback, redisplay, &data); 
  XtAddCallback(canvas, XmNresizeCallback, resize, &data);

  XtRealizeWidget(toplevel);
  XtMainLoop();
}

Pixmap create_logo(w, gc, bits, width, height)
   Widget   w;
   char    *bits;
#ifdef JUST_LIKE_BOOK
   int      width, height;
#else
   /*
   /* This is required in X11R4, and really should have been done that way in
   /* X11R3.  This is a common X11R3 -> X11R4 poprting problem.
   */
   Dimension      width, height;
#endif
{
   Pixmap    bitmap, pix;
  /*
   * Create a bitmap from the data.
   */
  bitmap = XCreateBitmapFromData(XtDisplay(w),  
                               RootWindowOfScreen(XtScreen(w)),
                               bits, width, height);
  /* 
   * Create a pixmap of the same depth as the default screen.
   */
  pix = XCreatePixmap(XtDisplay(w), 
                      RootWindowOfScreen(XtScreen(w)),
                      width, height,
                      DefaultDepthOfScreen(XtScreen(w)));
    /*
   * Copy the contents of plane 1 of the bitmap to the 
   * pixmap, using the widget's colors.
   */
  XCopyPlane(XtDisplay(w), bitmap, pix, gc, 0, 0, 
             xlogo64_width, xlogo64_height, 0, 0, 1);
  /*
   * We don't need the bitmap anymore, so free it.
   */
  XFreePixmap(XtDisplay(w), bitmap);
  return pix;
}


void redisplay (w, data, call_data)
     Widget        w;
     pixmap_data   *data;
     XmDrawingAreaCallbackStruct *call_data;
{
  Arg       wargs[2];
  Dimension widget_width, widget_height;
  int       n;
  /*
   * Get the current size of the widget window.
   */ 
  n = 0;
  XtSetArg(wargs[n], XtNwidth,  &widget_width); n++;
  XtSetArg(wargs[n], XtNheight, &widget_height);n++;
  XtGetValues(w, wargs, n);
  /*
   * Copy the contents of the pixmap to the 
   * center of the window.
   */
  XCopyArea(XtDisplay(w), data->pix, XtWindow(w), data->gc,
            0, 0, data->width, data->height, 
            (widget_width  - data->width) / 2,  
            (widget_height - data->height) / 2);
}

void resize (w, data, call_data)
   Widget                       w;
   caddr_t                      data;
   XmDrawingAreaCallbackStruct *call_data;
   {
    if(XtIsRealized(w))
      XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);
   }
