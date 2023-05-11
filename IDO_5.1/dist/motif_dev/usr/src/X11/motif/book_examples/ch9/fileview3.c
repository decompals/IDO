/**********************************************************************************
  * fileview.c: a file viewer, using compound strings
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



#include "fileview3.h" 

extern Widget create_scrollbar ();

main (argc, argv)
  int       argc;
  char     *argv[];
{
  Widget     toplevel, sb, sw;
  Arg        wargs[10];
  int        n;
  text_data  data;

  toplevel = XtInitialize (argv[0], "Fileview", 
                           NULL, 0, &argc, argv);
  XtGetApplicationResources (toplevel, &data, resources,
                              XtNumber (resources), NULL, 0);
  data.fontlist = XmFontListCreate (data.font, "Fileview_Charset");
 /*
  * Read the file specified in argv[1] into the
  * text buffer. 
  */
 load_file (&data, (argc == 2) ? argv[1] : NULL);
 /*
  * Create a ScrolledWindow widget as a base. 
  */
 sw = XtCreateManagedWidget ("framework", 
                             xmScrolledWindowWidgetClass,
                             toplevel, NULL, 0);
 /*
  * Create the drawing surface.
  */
 data.canvas= XtCreateManagedWidget("canvas",
                                    xmDrawingAreaWidgetClass,
                                    sw, NULL, 0);
 /*
  * Determine the initial size of the canvas and store it
  */
 XtSetArg (wargs[0], XtNheight, &data.canvas_height);
 XtSetArg (wargs[1], XtNwidth, &data.canvas_width);
 XtGetValues (data.canvas, wargs, 2);
 /*
  * Register callbacks for resizes and exposes.
  */
 XtAddCallback (data.canvas, XmNexposeCallback, 
                handle_exposures, &data);
 XtAddCallback (data.canvas, XmNresizeCallback, 
                resize, &data);
 /*
  * Create the scrollbar
  */
 data.scrollbar = create_scrollbar (sw, &data);
 /*
  * Registee the scrollbar and work area for the
  * ScrolledWindow widget.
  */
 n = 0;
 XtSetArg(wargs[n], XmNverticalScrollBar, data.scrollbar);n++;
 XtSetArg(wargs[n], XmNworkWindow, data.canvas); n++;
 XtSetValues(sw, wargs, n);

 XtRealizeWidget (toplevel);
 create_gc (&data);
 XtMainLoop ();
}


Widget create_scrollbar (parent, data)
  Widget        parent;
  text_data    *data;
{
  Arg    wargs[10];
  int    n = 0;
  Widget scrollbar;
  /*
   * Set the scrollbar so that movements are
   * reported in terms of lines of text. Set the
   * scrolling increment to a single line, and the page
   * increment to the number of lines the canvas widget
   * can hold. Also set the slider size to be proportional.
   */
  XtSetArg (wargs[n], XmNminimum, 0);  n++;
  XtSetArg (wargs[n], XmNmaximum, data->nitems);  n++;
  XtSetArg (wargs[n], XmNincrement, 1);  n++;	
#ifndef JUST_LIKE_BOOK
  XtSetArg (wargs[n], XmNsliderSize, data->canvas_height / data->fontheight);n++;
#else
  /*
   * Dumb. All this work just to find out it is equivalent to the number
   * of lines in the view area.
   */
  XtSetArg (wargs[n], XmNsliderSize, 
	            (data->canvas_height * data->canvas_height) / 
        	    (data->nitems * data->fontheight) );   n++;
#endif
  XtSetArg (wargs[n], XmNpageIncrement,
	            data->canvas_height/ data->fontheight);  n++;
  scrollbar = XtCreateManagedWidget ("scrollbar",
                                     xmScrollBarWidgetClass,
                                     parent, wargs, n);
  XtAddCallback (scrollbar, XmNvalueChangedCallback,
                 scroll_bar_moved, data);
  XtAddCallback (scrollbar, XmNdragCallback,
                 scroll_bar_moved, data);
  return scrollbar;
}

create_gc (data)
  text_data  *data;
{
  XGCValues  gcv;
  Display   *dpy  = XtDisplay(data->canvas);
  Window     w    = XtWindow(data->canvas);
  int        mask = GCFont | GCForeground | GCBackground;
  Arg        wargs[10];
  int        n;
  /*
   * Create a GC using the colors of the canvas widget.
   */
   n = 0;
   XtSetArg(wargs[n], XtNforeground, &gcv.foreground); n++;
   XtSetArg(wargs[n], XtNbackground, &gcv.background); n++;
   XtGetValues(data->canvas, wargs, n);
  
   gcv.font       = data->font->fid;
   data->gc       = XCreateGC (dpy, w, mask, &gcv);
}

void scroll_bar_moved(w, data, call_data)
     Widget     w;
     text_data *data;
     XmScrollBarCallbackStruct *call_data;
{
   int     sliderpos = call_data->value;
   int     ysrc,  redraw_top, delta;
   
   /* 
    * Compute number of pixels the text needs to be moved.
    */
   delta = ABS((data->top - sliderpos) * data->fontheight);   
   delta = MIN(delta, data->canvas_height);
   /*
    * If we are scrolling down, we start at zero and simply 
    * move by the delta. The portion that must be redrawn
    * is simply between zero and delta.
    */ 
   ysrc = redraw_top = 0;
   /* 
    * If we are scrolling up, we start at the delta and move
    * to zero. The part to redraw lies between the bottom
    * of the window and the bottom - delta.
    */
   if(sliderpos >= data->top){ 
     ysrc        =  delta;
     redraw_top  =  data->canvas_height - delta;
   }
   /*
    * Set the top line of the text buffer.
    */
   data->top = sliderpos;
   /*
    * Move the existing text to its new position.
    * Turn off any clipping on the GC first.
    */
   XSetClipMask(XtDisplay(w), data->gc, None);
   XCopyArea(XtDisplay(data->canvas), XtWindow(data->canvas), 
                       XtWindow(data->canvas), data->gc,
                       0, ysrc, 
                       data->canvas_width,  
                       data->canvas_height - delta,
                       0,  delta - ysrc);
   /*
    * Clear the remaining area of any old text, 
    * Request server to generate Expose events for the
    * area by setting exposures to TRUE.
    */
   XClearArea(XtDisplay(w), XtWindow(data->canvas),
	      0, redraw_top,
	      0, delta, TRUE); 
}



void resize (w, data, call_data)
  Widget          w;
  text_data      *data;
  caddr_t         call_data;
{
  Arg   wargs[10];
  int   n;
  /*
   * Determine the new widget of the canvas widget.
   */
  n = 0;
  XtSetArg (wargs[n], XtNheight, &data->canvas_height);n++;
  XtSetArg (wargs[n], XtNwidth,  &data->canvas_width);n++;
  XtGetValues (w, wargs, n);
  /*
   * Reset the scrollbar slider to indictae the relative
   * proportion of text displayed and also the new page size.
   */
  n = 0;
#ifndef JUST_LIKE_BOOK
  XtSetArg (wargs[n], XmNsliderSize, data->canvas_height / data->fontheight);n++;
#else
  /*
   * Dumb. All this work just to find out it is equivalent to the number
   * of lines in the view area.
   */
  XtSetArg (wargs[n], XmNsliderSize, 
	            (data->canvas_height * data->canvas_height) / 
        	    (data->nitems * data->fontheight) );  n++;
#endif
  XtSetArg (wargs[n], XmNpageIncrement, data->canvas_height / data->fontheight);  n++;
  XtSetValues (data->scrollbar, wargs, n);
}

load_file (data, filename)
  text_data      *data;
  char           *filename;
{
  XCharStruct     char_info;
  FILE           *fp, *fopen ();
  char            buf[MAXLINESIZE];
#ifdef JUST_LIKE_BOOK
  int             height, i;
#else
  /*
  /* this is a typical Motif 1.0 -> Motif 1.1 (or more accurately -> X11R4)
  /* porting issue to watch out for.
  */
  int             i;
  Dimension       height;
#endif
  /*
   * Open the file. 
   */
  if ((fp = fopen (filename, "r")) == NULL) {
    fprintf (stderr, "Unable to open %s\n", filename);
    exit (1);
  }
  /*
   * Read each line of the file into the buffer,
   * calculating and caching the extents of
   * each line. 
   */
  i = 0;
  while ((fgets (buf, MAXLINESIZE, fp)) != NULL && 
          i < MAXLINES) {
    buf[strlen (buf) - 1] = ' ';
    data->chars[i] = XmStringCreate(buf, "Fileview_Charset");
    XmStringExtent (data->fontlist, data->chars[i], 
		    &data->width[i], &height);
    data->fontheight = MAX(data->fontheight, height);
    i++;
  }
  /*
   * Close the file.
   */
  fclose(fp);
  /*
   * Set the number of lines, and initialize the
   * current line number to be 0.
   */
  data->nitems = i;
  data->top = 0;
}

void handle_exposures (w, data, cb)
   Widget          w;
   text_data      *data;
   XmDrawingAreaCallbackStruct *cb;
{
  int        yloc = 0, 
             index = data->top;
   Region  region;
  /*
   * Create a region and add the 
   * contents of the of the event
   */
  region = XCreateRegion();
  XtAddExposureToRegion(cb->event, region);
  /*
   * Set the clip mask of the GC.
   */
  XSetRegion (XtDisplay(w), data->gc, region); 
  /*
   * Redraw each line until the bottom of the
   * window is reached, or we run out of lines. 
   */
  while (index < data->nitems && yloc < data->canvas_height) {
       XmStringDrawImage (XtDisplay (w), XtWindow (w), 
			  data->fontlist,
			  data->chars[index],
			  data->gc, 
			  MARGIN, yloc, data->width[index], 
			  XmALIGNMENT_BEGINNING,
			  XmSTRING_DIRECTION_R_TO_L,
			  NULL);
    yloc += data->fontheight;
    index++;
  }
  /*
   * Free the region.
   */
  XDestroyRegion(region);
}
