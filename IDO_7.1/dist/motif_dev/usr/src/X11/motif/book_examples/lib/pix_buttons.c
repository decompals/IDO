 /*************************************************
  *    pixmap_browser functions
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  200-202
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
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>
#include "libXs.h"


 Widget 
xs_create_pixmap_browser (parent,tiles,n_tiles,callback,data)
   Widget     parent;       /* widget to manage the browser */
   char      *tiles[];      /* list of tile names           */
   int        n_tiles;      /* how many tiles               */
   void      (*callback)(); /* invoked when state changes   */
   caddr_t    data;         /* data to be passed to callback*/
{
  Widget     browser;
  WidgetList buttons;
  int        i;
  Arg        wargs[10];
  /*
   * Malloc room for button widgets.
   */
  buttons = (WidgetList) XtMalloc(n_tiles * sizeof(Widget));
  /*
   * Create a "RadioBox" RowColumn widget.
   */
  XtSetArg(wargs[0], XmNentryClass, xmToggleButtonWidgetClass);
  browser = XmCreateRadioBox(parent, "browser", wargs, 1);
  /*
   * Create a button for each tile. If a callback function
   * has been given, register it as an XmNvalueChangedCallback
   */
  for(i=0;i< n_tiles;i++){
   buttons[i] = xs_create_pixmap_button(browser, tiles[i]);
   if(callback)
     XtAddCallback(buttons[i], XmNvalueChangedCallback, 
                   callback, data);
 }
 /*
  * Manage all buttons and return the RadioBox widget, which
  * is still unmanaged
  */ 
 XtManageChildren(buttons, n_tiles);
 return browser;
}

Widget xs_create_pixmap_button(parent, pattern)
     Widget         parent;
     char *         pattern;
{
   Pixmap   tile;
   Widget   button;
   Arg      wargs[10];
   Pixel    foreground, background;
   int      n;

   button = XtCreateWidget(pattern, xmToggleButtonWidgetClass,
                           parent, NULL, 0);
   /*
    * Retrieve the colors of the widget.
    */
   n = 0;
   XtSetArg(wargs[n], XtNforeground, &foreground);n++;
   XtSetArg(wargs[n], XtNbackground, &background);n++;
   XtGetValues(button, wargs, n);
   /*
    * Get a tile corresponding the given pattern.
   */
   tile = XmGetPixmap (XtScreen(button),  pattern,
                       foreground, background);
   /*
    * Display the pixmap in the button and also store it
    * so it can be retrieved from the button later.
    */
   n = 0;
   XtSetArg(wargs[n], XmNlabelType,   XmPIXMAP); n++;
   XtSetArg(wargs[n], XmNlabelPixmap, tile); n++;
   XtSetArg(wargs[n], XmNuserData,    tile); n++;
   XtSetValues(button, wargs, n);
   /*
    * Return the unmanaged button.
    */
   return button;
}

xs_register_pattern(w, name, bits, width, height)
  Widget         w;
  char          *name;
  unsigned char *bits;
#ifdef JUST_LIKE_BOOK
  int            width, height;
#else
   /*
   /* This is required in X11R4, and really should have been done that way in
   /* X11R3.  This is a common X11R3 -> X11R4 poprting problem.
   */
  Dimension      width, height;
#endif
 {
    XImage *image;
    image = XCreateImage(XtDisplay(w),
                         DefaultVisualOfScreen(XtScreen(w)), 
                      1, XYBitmap, 0, 
                         bits, width, height, 8, 2);
    XmInstallImage(image, name);
   }
