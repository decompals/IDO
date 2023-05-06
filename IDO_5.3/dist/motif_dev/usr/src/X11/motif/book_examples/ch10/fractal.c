/**********************************************************************************
  * fractal.c: A simple fractal generator
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  247-253
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

#include "fractal.h"

main(argc, argv)
     int   argc;
     char *argv[];
{
  Widget     toplevel, canvas;
  image_data data;
  toplevel = XtInitialize(argv[0], "Fractal", NULL, 0, 
                          &argc, argv);
 /*
  *  Add the string to float type converter.
  */
 XtAddConverter(XtRString, XtRFloat, xs_cvt_str_to_float, 
                NULL, 0);
 XtGetApplicationResources(toplevel, &data, resources, 
                           XtNumber(resources), NULL, 0);
 /*
  * Create the widget to display the fractal and register
  * callbacks for resize and refresh.
  */
 canvas = XtCreateManagedWidget("canvas", 
                                xmDrawingAreaWidgetClass, 
                                toplevel, NULL, 0);
 init_data(canvas, &data);  
 XtAddCallback(canvas, XmNexposeCallback, redisplay, &data); 
 XtAddCallback(canvas, XmNresizeCallback, resize, &data); 
 XtRealizeWidget(toplevel);
 resize(canvas, &data, NULL);
 XtMainLoop();
}

init_data(w, data)
    Widget      w;
    image_data *data;
{
  int y;
  Arg wargs[2];
  /*
   * Get the size of the drawing area.
   */
  XtSetArg(wargs[0], XtNwidth,  &data->width);
  XtSetArg(wargs[1], XtNheight, &data->height);
  XtGetValues(w, wargs,2);
  /*
   * Find out how many colors we have to work with, and  
   * create a default, writable, graphics context.
   */
  data->ncolors = XDisplayCells(XtDisplay(w), 
                                XDefaultScreen(XtDisplay(w)));
  data->gc = XCreateGC(XtDisplay(w),
                       DefaultRootWindow(XtDisplay(w)),
                       NULL, NULL); 
  /*
   *  Initialize the pixmap to NULL.
   */
  data->pix = NULL;
}

void create_image (w, data)
     Widget          w;
     image_data     *data;
{
 int  x, y, iteration;
 /*
  * For each pixel on the window....
  */
 for (y = 0; y < data->height; y++) {
  for (x = 0; x < data->width; x++) {
    complex z, k;
   /*
    * Initialize K to the normalized, floating coordinate
    * in the x, y plane. Init Z to (0.0, 0.0).
    */
    z.real =  z.imag = 0.0;
    k.real =  data->origin.real + (float) x / 
                 (float) data->width * data->range;
    k.imag =  data->origin.imag - (float) y / 
                 (float) data->height * data->range;
   /*
    * Calculate z = (z + k) * (z + k) over and over.
    */
   for (iteration = 0; iteration < data->depth; iteration++){
      float   distance, real_part, imag_part;

      real_part = z.real + k.real;
      imag_part = z.imag + k.imag;
      z.real = real_part * real_part - imag_part * imag_part;
      z.imag = 2 * real_part * imag_part;
      distance  = z.real * z.real + z.imag * z.imag;     
     /*
      * If the z point has moved off the plane, set the 
      * current foreground color to the distance (coerced to 
      * an int and modulo the number of colors available),
      * and draw a point in both the window and the pixmap.
      */
     if (distance  >= data->max_distance){ 
       int color = (int) distance % data->ncolors;

       XSetForeground(XtDisplay(w), data->gc, color);
       XDrawPoint (XtDisplay(w), data->pix, data->gc, x, y);
       if(XtIsRealized(w))
        XDrawPoint (XtDisplay(w), XtWindow(w), data->gc,x,y);
       break;
     }
   }
  }
 }
}

void redisplay (w, data, call_data)
     Widget          w;
     image_data     *data;
     XmDrawingAreaCallbackStruct    *call_data;
{
 XExposeEvent  *event = (XExposeEvent *) call_data->event;
 /*
  * Extract the exposed area from the event and copy
  * from the saved pixmap to the window.
  */
  XCopyArea(XtDisplay(w), data->pix, XtWindow(w), data->gc, 
            event->x, event->y, event->width, event->height, 
            event->x, event->y);
}

void resize(w, data, call_data)
     Widget         w;
     image_data    *data;
     caddr_t        call_data;
{
  Arg wargs[10];
  /*
   *  Get the new window size.
   */
  XtSetArg(wargs[0], XtNwidth,  &data->width);
  XtSetArg(wargs[1], XtNheight, &data->height);
  XtGetValues(w, wargs, 2);
  /*
   * Clear the window.
   */
   if(XtIsRealized(w))
       XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, TRUE);
  /*
   *  Free the old pixmap and create a new pixmap 
   *  the size of the window.
   */
  if(data->pix)
     XFreePixmap(XtDisplay(w), data->pix);
  data->pix= XCreatePixmap(XtDisplay(w),
                           DefaultRootWindow(XtDisplay(w)),
                           data->width, data->height, 
                           DefaultDepthOfScreen(XtScreen(w)));
  XSetForeground(XtDisplay(w), data->gc,
                 BlackPixelOfScreen(XtScreen(w)));
  XFillRectangle(XtDisplay(w), data->pix, data->gc, 0, 0, 
                 data->width,  data->height);
  /*
   * Generate a new image.
    */
  create_image(w, data);
}

