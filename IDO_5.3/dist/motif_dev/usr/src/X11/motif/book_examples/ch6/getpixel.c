/**********************************************************************************
  * getpixel.c: Test the rgb functions from chapter 6
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:   ??
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

#include <X11/Intrinsic.h> 

main(argc, argv)
    int   argc;
    char *argv[];
  {
    Widget toplevel;
    int red, green, blue;
    /*
     * Initialize the Intrinsics.
     */
    toplevel = XtInitialize(argv[0], "Getpixel", NULL, 0, 
                            &argc, argv);
    /*
     * Create a Core widget.
     */
    XtCreateManagedWidget("widget", widgetClass, 
                           toplevel, NULL, 0);

    printf("%d\n", get_pixel(toplevel, 1000, 2000, 3000));
    printf("%d\n", get_pixel_by_name(toplevel, "Red"));
    load_rgb(toplevel, &red, &green, &blue);
    printf("%d %d %d\n", red, green, blue);
    XtRealizeWidget(toplevel);

    XtMainLoop();
}

get_pixel(w, red, green, blue)
   Widget  w;
   int     red, green, blue;
{
   Display *dpy =  XtDisplay(w);
   int      scr =  DefaultScreen(dpy);
   Colormap cmap = DefaultColormap(dpy, scr);
   XColor   color;
   /* 
    * Fill in the color structure.
    */
   color.red   = red;
   color.green = green;
   color.blue  = blue;
   /* 
    * Try to allocate the color.
    */
   if(XAllocColor(dpy, cmap, &color))
      return (color.pixel);
   else {
      printf("Warning: Couldn't allocate requested color\n");
      return (BlackPixel(dpy, scr));
   }
}
load_rgb(w, red, green, blue)
   Widget  w;
   int    *red, *green, *blue;
{
   Display *dpy  = XtDisplay(w);
   int      scr  = DefaultScreen(dpy);
   Colormap cmap = DefaultColormap(dpy, scr);
   XColor   color;
   int      cells[3];
  /*
   *  Try to allocate three consecutive color cells.
   */
  if(XAllocColorCells(dpy, cmap, True, 
                      NULL, 0, cells, 3)){
    /* 
          *  If successful, create a red color struct, and store
     *  it in the first allocated cell. 
     */
    color.red = 65535;
    color.green = color.blue = 0;
    *red = color.pixel = cells[0];
    XStoreColor(dpy, cmap, &color);
    /*
     *  Store Green in the second cell.
     */
    color.green = 65535;
    color.red =  color.blue = 0;
    *green = color.pixel = cells[1];
    XStoreColor(dpy, cmap, &color);
    /* 
     * Store Blue in the second cell.
     */
    color.blue = 65535;
    color.red = color.green = 0;
    *blue = color.pixel = cells[2];
    XStoreColor(dpy, cmap, &color);
  }
  else{
    printf("Warning:Couldn't allocate color cells\n");
    *blue = *red = *green = BlackPixel(dpy, scr);
  }
}
get_pixel_by_name(w, colorname)
   Widget w;
   char  *colorname;
{
  Display *dpy  = XtDisplay(w);
  int      scr  = DefaultScreen(dpy);
  Colormap cmap = DefaultColormap(dpy, scr);
  XColor   color, ignore;
  /* 
   * Allocate the named color.
   */
  if(XAllocNamedColor(dpy, cmap, colorname, &color, &ignore))
    return (color.pixel);
  else{
    printf("Warning: Couldn't allocate color %s\n", colorname);
    return (BlackPixel(dpy, scr));
  }
}
