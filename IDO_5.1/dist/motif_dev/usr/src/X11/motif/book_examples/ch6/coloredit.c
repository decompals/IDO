/**********************************************************************************
  * coloredit.c: A simple color editor.
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:   178-188 
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


#include "coloredit.h"

main(argc, argv)
  int    argc;
  char  *argv[];
{
  Widget    toplevel, bb, sliders, rc;
  Colormap  def_colormap;
  XColor    Colors[MAXCOLORS];
  int       i;
 /*
  * Initialize the Intrinsics and save pointer to the display.
  */
  toplevel = XtInitialize(argv[0], "Coloredit", NULL, 0,
                          &argc, argv);
  dpy = XtDisplay(toplevel);
  /*
   * Determine the number of colors to be edited.
   */
  ncolors = DisplayCells(dpy, DefaultScreen(dpy));
if(ncolors > MAXCOLORS) ncolors = MAXCOLORS;
  /* 
   * Create a base to hold everything.
   */
  rc = XtCreateManagedWidget("base",xmRowColumnWidgetClass, 
                             toplevel, NULL, 0);
  /* 
   * Create a grid of buttons, one for each 
   * color to be edited.
   */
  create_color_bar(rc);
  /*
   * A separator widget looks nice, between the colors
   * and the controls.
   */
  XtCreateManagedWidget("sep", xmSeparatorWidgetClass,
                        rc, NULL, 0);
  /*
   * Put the controls inside an XmBulletinBoard widget
   */
  bb = XtCreateManagedWidget("controls",
                             xmBulletinBoardWidgetClass,
                             rc, NULL, 0);
  /*
   * Use an XmLabel widget to display the current rgb values.
   */
 color_display_panel = 
         XtCreateManagedWidget("display",
                               xmLabelWidgetClass, 
                         					      bb, NULL,0);
  /*
   * Create a row column widget containing three sliders,
   * one for each color component.
   */
  sliders = XtCreateManagedWidget("sliderpanel",
                                   xmRowColumnWidgetClass,
                                   bb, NULL, 0);
  red_slider   = make_slider("red", sliders,
                             red_slider_moved);
  green_slider = make_slider("green", sliders,
                             green_slider_moved);
  blue_slider  = make_slider("blue",  sliders,
                            blue_slider_moved);
  /* 
   * Add a quit button.
   */
  xs_create_quit_button(bb);
  /*
   * Get the ID of the default colormap.
   */
  def_colormap = DefaultColormap(dpy, DefaultScreen(dpy));
  for( i = 0; i < ncolors; i++ ) {
    Colors[i].pixel = i;
    Colors[i].flags = DoRed|DoGreen|DoBlue;
  }
  XQueryColors(dpy, def_colormap, Colors, ncolors);
  my_colormap = 
       XCreateColormap(dpy, DefaultRootWindow(dpy), 
                       DefaultVisual(dpy, DefaultScreen(dpy)),
                       AllocAll);
  XStoreColors(dpy, my_colormap, Colors, ncolors);
  /*
   * Initialize the pixel member of the global color struct
   * To the first editable color cell.
   */
  current_color.pixel = 0;

  XtRealizeWidget(toplevel);

  XSetWindowColormap(dpy, XtWindow(toplevel), my_colormap);

  XtMainLoop();
}

Widget make_slider(name, parent, callback) 
  char  *name;
  Widget parent;
  void (*callback)();
{
   Widget  w;
  int     n;
  Arg     wargs[10];
  /*
   * Create an XmScale widget.
   */
  n = 0;
  XtSetArg(wargs[n], XmNminimum, 0); n++;
  XtSetArg(wargs[n], XmNmaximum, 65535); n++;

  w = XtCreateManagedWidget(name, xmScaleWidgetClass, 
                            parent, wargs, n);
  /*
   * Add callbacks to be invoked when the slider moves.
   */
  XtAddCallback(w, XmNvalueChangedCallback, callback,NULL);
  XtAddCallback(w, XmNdragCallback, callback, NULL);

  return (w);
}

Widget create_color_bar(parent)
  Widget parent;
{
  Widget      panel;
  WidgetList  colors;
  int         i, n;
  char        name[10];
  Arg         wargs[10];
 colors = (WidgetList) XtMalloc( ncolors * sizeof(Widget));
  
  /*
   * Create the row column manager to hold all 
   * color buttons.
   */
  n = 0;
  panel = XtCreateManagedWidget("colorpanel",
                                xmRowColumnWidgetClass,
                                parent, wargs, n);
  /* 
   * Create ncolors widgets. Use the relative color 
   * cell number as the name of each color. Add a 
   * XmNactivateCallback for each cell with the color 
   * index as client_data.
   */
  for(i=0;i<ncolors;i++){
   n = 0;
   XtSetArg(wargs[n], XtNbackground, i); n++; 
   sprintf(name,"%d",i);
   colors[i] = XtCreateWidget(name, xmLabelWidgetClass, 
                              panel, wargs, n);
   XtAddEventHandler(colors[i], ButtonPressMask, False,
                     set_current_color, i);
   }
   XtManageChildren(colors, ncolors);
  
   return panel;
}

void red_slider_moved(w, client_data, call_data)
     Widget   w;
     caddr_t  client_data;
     XmScaleCallbackStruct *call_data;
{
 /*
  * Set the red color components of  the global 
  * current_color structure.
  */
  current_color.red =  call_data->value;
  /*
   * Update the digital rgb display and the current 
   * color label.
   */
  update_color();
}

void blue_slider_moved(w, client_data, call_data)
     Widget   w;
     caddr_t  client_data;
     XmScaleCallbackStruct *call_data;
{
 /*
  * Set the blue color components of  the global 
  * current_color structure.
  */
  current_color.blue =  call_data->value;
  /*
   * Update the digital rgb display and the current 
   * color label.
   */
  update_color();
}

void green_slider_moved(w, client_data, call_data)
     Widget   w;
     caddr_t  client_data;
     XmScaleCallbackStruct *call_data;
{
 /*
  * Set the green color components of  the global 
  * current_color structure.
  */
  current_color.green =  call_data->value;
  /*
   * Update the digital rgb display and the current 
   * color label.
   */
  update_color();
}

update_color()
{
  Arg  wargs[1];
  char str[25];

  /*
   * Update the digital display.
   */
xs_wprintf(color_display_panel, "%3d %3d %3d", 
           current_color.red, current_color.green, 
           current_color.blue);
 /*
  * Update the current color.
  */
  XStoreColor(dpy, my_colormap, &current_color); 
}

void set_current_color(w, number, event)
  Widget   w;
  int      number;
  XEvent  *event;
{
  Arg wargs[10];
  current_color.flags = DoRed | DoGreen | DoBlue;
  /*
   * Get the current color components of the selected button.
   */
  current_color.pixel = number;
  XQueryColor(dpy, my_colormap, &current_color);
  /*
   * Use each color component to as the new 
   * position of the corresponding slider.
   */
  XtSetArg(wargs[0], XmNvalue, current_color.red);
  XtSetValues(red_slider, wargs, 1);

  XtSetArg(wargs[0], XmNvalue, current_color.green);
  XtSetValues(green_slider, wargs, 1);

  XtSetArg(wargs[0], XmNvalue, current_color.blue);
  XtSetValues(blue_slider, wargs, 1);
#ifndef JUST_LIKE_BOOK
  update_color();
#endif
}
