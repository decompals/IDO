/**********************************************************************************
  * draw.c: a simple graphics drawing program.
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


#include <stdio.h>
#include "draw.h"

main(argc, argv)
   int   argc;
   char *argv[];
{
  Widget        toplevel, canvas, framework, command, tiles;
  graphics_data data;
  int           n;
  Arg           wargs[10];
    
  toplevel = XtInitialize(argv[0], "Draw", NULL, 0, 
                          &argc, argv);
  framework = XtCreateManagedWidget("framework", 
                                    xmFormWidgetClass, 
                                    toplevel, NULL, 0);
  /*
   * Create the column to hold the commands.
   */
  command = XtCreateManagedWidget("command", 
                                  xmRowColumnWidgetClass, 
                                  framework, NULL, 0);
  /*
   * Create the drawing surface and add the 
   * rubber banding callbacks.
   */
  canvas = XtCreateManagedWidget("canvas",
                                 xmDrawingAreaWidgetClass, 
                                 framework, NULL, 0);
  XtAddCallback(canvas, XmNexposeCallback, refresh, &data);

  XtAddEventHandler(canvas, ButtonPressMask, FALSE,
                    start_rubber_band, &data);
  XtAddEventHandler(canvas, ButtonMotionMask, FALSE,
                    track_rubber_band, &data);
  XtAddEventHandler(canvas, ButtonReleaseMask, FALSE,
                    end_rubber_band, &data);
  n = 0;
  XtSetArg(wargs[n], XmNtopAttachment,    XmATTACH_FORM);n++;
  XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_FORM);n++;
  XtSetArg(wargs[n], XmNleftAttachment,   XmATTACH_FORM);n++;
  XtSetValues(command, wargs, n);

  n = 0;
  XtSetArg(wargs[n], XmNtopAttachment,   XmATTACH_FORM);  n++;
  XtSetArg(wargs[n], XmNbottomAttachment,XmATTACH_FORM);  n++;
  XtSetArg(wargs[n], XmNleftAttachment,  XmATTACH_WIDGET);n++;
  XtSetArg(wargs[n], XmNleftWidget,     command);         n++;
  XtSetArg(wargs[n], XmNrightAttachment,  XmATTACH_FORM); n++;
  XtSetValues(canvas, wargs, n);
  /*
   * Initialize the graphics buffer and other data.
   */
  init_data(canvas, &data);
  /*
   * Add a quit button.
   */
  xs_create_quit_button(command);
  /*
   * Add the drawing command panel.
   */ 
  create_drawing_commands(command, &data);
  /*
   * Add a palette of fill patterns.
   */
  xs_register_pattern(toplevel, "foreground", fg_bitmap,
                     fg_width, fg_height);
  tiles = xs_create_pixmap_browser(command, 
                                   patterns,
                                   XtNumber(patterns), 
                                   set_fill_pattern, &data);
  XtManageChild(tiles);
  XtRealizeWidget(toplevel);
  /*
   * Establish a passive grab on the drawing canvas window.
   */
  XGrabButton(XtDisplay(canvas), AnyButton, AnyModifier, 
              XtWindow(canvas), TRUE, 
              ButtonPressMask | ButtonMotionMask | 
              ButtonReleaseMask,
              GrabModeAsync, GrabModeAsync,
              XtWindow(canvas), 
              XCreateFontCursor(XtDisplay(canvas),
                                XC_crosshair));
  XtMainLoop();
}

struct {
  char   *name;
  void   (*func)();
}  command_info[] = {
      "Line",             draw_line,
      "Circle",           draw_circle,
      "Rectangle",        draw_rectangle,
      "Filled Circle",    draw_filled_circle,
      "Filled Rectangle", draw_filled_rectangle
 };

create_drawing_commands(parent, data)
     Widget         parent;
     graphics_data *data;
{
  Widget  w, commands;
  Arg     wargs[2];
  int     i;
  /*
   * Group all commands in a column.
   */
  XtSetArg(wargs[0], XmNentryClass, xmToggleButtonWidgetClass);
    commands = XmCreateRadioBox(parent, "commands", 
                              wargs, 1);
  XtManageChild(commands);
  /*
   * Create a button for each drawing function.
   */
  for(i=0;i < XtNumber(command_info); i++){
    XtSetArg(wargs[0], XmNuserData, command_info[i].func);
    w = XtCreateManagedWidget(command_info[i].name,
                              xmToggleButtonWidgetClass, 
                              commands, wargs, 1);
    XtAddCallback(w, XmNvalueChangedCallback, activate, data);
  }
}

void set_fill_pattern(w, data, call_data)
     Widget         w;
     graphics_data *data;
     XmToggleButtonCallbackStruct  *call_data;
{
  Pixmap    tile;
  int       i;
  XGCValues values;
  Arg       wargs[1];

 static int mask = GCForeground | GCBackground | 
                   GCTile | GCFillStyle;

 if(call_data->reason==XmCR_VALUE_CHANGED && call_data->set){
   XtSetArg(wargs[0], XmNuserData, &tile);
   XtGetValues(w, wargs, 1);
   /* 
    * Get a GC using this tile pattern 
    */
   values.foreground = data->foreground;
   values.background = data->background;
   values.fill_style = FillTiled;
   values.tile       = tile;
   data->gc = XtGetGC(w, mask, &values);   
 }
}

init_data(w, data)
     Widget          w;
     graphics_data  *data;
{
  XGCValues values;
  Arg       wargs[5];
  data->current_func = NULL;
  data->next_pos     = 0;
  /*
   * Get the colors the user has set for the widget. 
   */
  XtSetArg(wargs[0], XtNforeground, &data->foreground);
  XtSetArg(wargs[1], XtNbackground, &data->background);
  XtGetValues(w, wargs,2);
  /*
   * Fill in the values structure
   */  
  values.foreground = data->foreground;
  values.background = data->background;
  values.fill_style = FillTiled;
  /*
   * Get the GC used for drawing.
   */
  data->gc= XtGetGC(w, GCForeground | GCBackground |
                       GCFillStyle, &values);
  /*
   * Get a second GC in XOR mode for rubber banding.
   */
  data->xorgc = xs_create_xor_gc(w);
}

void start_rubber_band(w, data, event)
        Widget          w;
        graphics_data  *data;
        XEvent         *event;
{
  if(data->current_func){
    /*
     * Store the starting point and draw the initial figure.
     */
    data->last_x = data->start_x = event->xbutton.x;
    data->last_y = data->start_y = event->xbutton.y;
    (*(data->current_func))(w, data->xorgc,
                            data->start_x, data->start_y, 
                            data->last_x, data->last_y);
  }
}

void track_rubber_band(w, data, event)
        Widget          w;
        graphics_data  *data;
        XEvent         *event;
{ 
 if(data->current_func){
   /*
    * Erase the previous figure.
    */
   (*(data->current_func))(w, data->xorgc,
                           data->start_x, data->start_y, 
                           data->last_x, data->last_y);
    /*
     * Update the last point.
     */
    data->last_x  =  event->xbutton.x;
    data->last_y  =  event->xbutton.y;
    /*
     * Draw the figure in the new position.
     */
    (*(data->current_func))(w, data->xorgc,
                            data->start_x, data->start_y, 
                            data->last_x, data->last_y);
  }
}

void end_rubber_band(w, data, event)
        Widget          w;
        graphics_data  *data;
        XEvent         *event;
{
  if(data->current_func){
    /*
     * Erase the XOR image. 
     */
    (*(data->current_func))(w, data->xorgc,
                            data->start_x, data->start_y, 
                            data->last_x, data->last_y);
    /*
     * Draw the figure using the normal GC. 
     */
    (*(data->current_func))(w, data->gc,
                            data->start_x, data->start_y, 
                            event->xbutton.x, 
                            event->xbutton.y);
    /*
     * Update the data, and store the object in 
     * the graphics buffer.
     */
    data->last_x  =  event->xbutton.x;
    data->last_y  =  event->xbutton.y;
    store_object(data);
  }
}

void draw_line(w, gc, x, y, x2, y2)
     Widget  w;
     GC      gc;
     int     x, y, x2, y2;
{
  Display *dpy = XtDisplay(w);
  Window   win = XtWindow(w);
  XDrawLine(dpy, win, gc, x, y, x2, y2);
}

void draw_rectangle(w, gc, x, y, x2, y2)
     Widget w;
     GC     gc;
     int    x, y, x2, y2;
{
  Display *dpy = XtDisplay(w);
  Window   win = XtWindow(w);

  check_points(&x, &y, &x2, &y2);
  XDrawRectangle(dpy, win, gc,  x, y, x2 - x, y2 - y);
}

void draw_filled_rectangle(w, gc, x, y, x2, y2)
     Widget  w;
     GC      gc;
     int     x, y, x2, y2;
{
  Display *dpy = XtDisplay(w);
  Window   win = XtWindow(w);

  check_points(&x, &y, &x2, &y2);
  XFillRectangle(dpy, win, gc, x, y, x2 - x, y2 - y);
}

check_points(x, y, x2, y2)
   int *x, *y, *x2, *y2;
{
  if(*x2 < *x){ int tmp = *x; *x = *x2; *x2 = tmp;}
  if(*y2 < *y){ int tmp = *y; *y = *y2; *y2 = tmp;}
}

void draw_circle(w, gc, x, y, x2, y2)
     Widget  w;
     GC      gc;
     int     x, y, x2, y2;
{
  Display *dpy = XtDisplay(w);
  Window   win = XtWindow(w);

  check_points(&x, &y, &x2, &y2);
  XDrawArc(dpy, win, gc, x, y, x2 - x, y2 - y, 0, 64 * 360);
}

void draw_filled_circle(w, gc, x, y, x2, y2)
     Widget  w;
     GC      gc;
     int     x, y, x2, y2;
{
  Display *dpy = XtDisplay(w);
  Window   win = XtWindow(w);

  check_points(&x, &y, &x2, &y2);
  XFillArc(dpy, win, gc, x, y, x2 - x, y2 - y, 0, 64 * 360);
}

void activate(w, data, call_data)
     Widget          w;
     graphics_data  *data;
     XmToggleButtonCallbackStruct   *call_data;
{
  int (*func)();
  Arg wargs[5];

  if(!call_data->set) return;

  XtSetArg(wargs[0], XmNuserData, &func);
  XtGetValues(w, wargs, 1);
  data->current_func = func; 
}

store_object(data)
     graphics_data *data;
{
  /* 
   * Check for space.
   */
  if(data->next_pos >= MAXOBJECTS){
   printf("Warning: Graphics buffer is full\n");
   return;
  }
  /*
   * Save everything we need to draw this object again.
   */
  data->buffer[data->next_pos].x1 = data->start_x;
  data->buffer[data->next_pos].y1 = data->start_y;
  data->buffer[data->next_pos].x2 = data->last_x;
  data->buffer[data->next_pos].y2 = data->last_y;
  data->buffer[data->next_pos].func = data->current_func;
  data->buffer[data->next_pos].gc = data->gc;
  /*
   * Increment the next position index.
   */
  data->next_pos++;
}

void refresh(w, data, call_data)
   Widget          w;
   graphics_data  *data;
   caddr_t         call_data;
{
  int i;
  for(i=0;i<data->next_pos;i++)
    (* (data->buffer[i].func))(w, data->buffer[i].gc,
                              data->buffer[i].x1,
                              data->buffer[i].y1,
                              data->buffer[i].x2,
                              data->buffer[i].y2);
}
