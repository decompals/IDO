/**********************************************************************************
  *  monitordata.c: display the data set by controldata
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  290-294
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

#include "data.h"

Widget    make_display();

main(argc, argv)
        int             argc;
        char           *argv[];
{
  Widget      toplevel, rc, speed, direction,  altitude;
  Window      root;
  XEvent      event;
  /* 
   * Initialize the Intrinsics,saving the default root window.
   */
  toplevel = XtInitialize(argv[0], "Monitordata", NULL, 0, 
                          &argc, argv);
  root =  DefaultRootWindow(XtDisplay(toplevel));
  /*
   * Initialize the Atoms used for the properties.
   */
  create_atoms(toplevel);
  rc = XtCreateManagedWidget("panel", xmRowColumnWidgetClass,
                              toplevel,  NULL, 0);
  /*
   * Create the display widgets.
   */
  speed      = make_display("speed",     rc);
  direction  = make_display("direction", rc);
  altitude   = make_display("altitude",  rc);
  xs_create_quit_button(rc);

  XtRealizeWidget(toplevel);
  /*
   * Request property change event for the ROOT window.
   */
  XSelectInput(XtDisplay(toplevel), root, PropertyChangeMask);
  /*
   *  Get the initial value of the data.
   */
  update_data(speed, direction, altitude);
  /*
   * We must use our own event loop to get properties 
   * events for the ROOT window.
   */
  while(TRUE){
    XtNextEvent(&event);
    /*
     * Check for property change events on the ROOT window
     * before dispatching the event through the Intrinsics.
     */
    switch (event.type){
      case PropertyNotify:
        if(event.xproperty.window == root &&
           event.xproperty.atom == FLIGHT_DATA)
         update_data(speed, direction, altitude);
        else
         XtDispatchEvent(&event);
        break;
      default:
         XtDispatchEvent(&event);
    }
  }
}


update_data(speed, direction, altitude)
     Widget speed, direction, altitude;
{
  int            type, format, nitems, left;
  flight_data   *retdata;
  char           str[100];
  Arg            wargs[1];
  /*
   * Retrieve the data from the root window property.
   */
  if(XGetWindowProperty(XtDisplay(speed), 
                        DefaultRootWindow(XtDisplay(speed)),
                        FLIGHT_DATA, 0, sizeof(flight_data), 
                        FALSE, FLIGHT_DATA_TYPE,
                        &type, &format, &nitems, &left,
                        &retdata) == Success &&
       type ==FLIGHT_DATA_TYPE){
    /*
     * If the data exists, display it.
     */
    xs_wprintf(speed,    "%d",     retdata->speed);
    xs_wprintf(direction, "%d",    retdata->angle);
    xs_wprintf(altitude,  "%5.1f", retdata->altitude + 0.05);
    XFree(retdata);
  }
}

Widget make_display(name, parent)
   char     *name;
   Widget    parent;
{
 Widget rc, w;
 /*
  * Create an XmRowColumn widget containing two
  * XmLabelwidgets.
  */
 rc = XtCreateManagedWidget(name, xmRowColumnWidgetClass, 
                            parent, NULL, 0);
 XtCreateManagedWidget("label", xmLabelWidgetClass, 
                       rc, NULL, 0);  
 w = XtCreateManagedWidget("display", xmLabelWidgetClass, 
                           rc, NULL, 0);
 return (w);
}

create_atoms(w)
    Widget w;
{
 Display * dpy = XtDisplay(w);
 FLIGHT_DATA      = XInternAtom(dpy, "Flight Data",      0);
 FLIGHT_DATA_TYPE = XInternAtom(dpy, "Flight Data Type", 0);
}
