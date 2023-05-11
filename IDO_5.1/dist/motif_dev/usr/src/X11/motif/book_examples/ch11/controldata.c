/**********************************************************************************
 *  controldata.c: The data controller using properties
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  285-290
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

void       slider_moved();
Widget     speed_ctl, angle_ctl, temp_ctl;
Widget     create_control();
Widget     make_controller();
main(argc, argv)
     int             argc;
     char          *argv[];
{
  Widget          toplevel, row_col;
  flight_data    data;

  data.speed = data.angle = data.altitude = 0;
  toplevel = XtInitialize(argv[0], "Controldata", NULL, 0, 
                          &argc, argv);
  /*
   * Create the atoms to represent the properties 
   * used to store the data.
   */  
  create_atoms(toplevel);
  row_col = XtCreateManagedWidget("panel", 
                                  xmRowColumnWidgetClass,
                                  toplevel, NULL, 0);
  /*
   *  Make three columns, each containing a label and a 
   *  slider control to control: speed, direction, 
   *  and altitude.
   */	   
  speed_ctl = make_controller("speed",     MAX_SPEED, 
                              row_col, &data);
  angle_ctl = make_controller("direction", MAX_ANGLE, 
                              row_col, &data);
  temp_ctl  = make_controller("altitude",  MAX_ALT,  
                              row_col, &data);
  xs_create_quit_button(row_col);

  XtRealizeWidget(toplevel);
  XtMainLoop();
}

create_atoms(w)
    Widget w;
{
 Display * dpy = XtDisplay(w);
 FLIGHT_DATA      = XInternAtom(dpy, "Flight Data",      0);
 FLIGHT_DATA_TYPE = XInternAtom(dpy, "Flight Data Type", 0);
}

Widget make_controller(name, max, parent, data)
   char          *name;
   int            max;
   Widget         parent;
   flight_data  *data;   
{
 Widget rc, w;
 /*
  * Create an XmRowColumn widget to manage a single 
  * control and a label.
  */
 rc = XtCreateManagedWidget(name, xmRowColumnWidgetClass, 
                            parent, NULL, 0);
 XtCreateManagedWidget("label", xmLabelWidgetClass, 
                       rc, NULL, 0);  
 w = create_control(rc, "control", 0, max, data);
 
 return (w);
}

Widget create_control(parent, name, minimum, maximum, data)
     Widget        parent;
     char         *name;
     int           minimum, maximum;
     flight_data *data;
{
  int    n;
  Arg    wargs[2];
  Widget w;
  /*
   * Create a scroll bar with range minimum to maximum. 
   */
  n = 0;
  XtSetArg(wargs[n], XmNminimum, minimum); n++;
  XtSetArg(wargs[n], XmNmaximum, maximum); n++;
  w = XtCreateManagedWidget(name, xmScrollBarWidgetClass, 
                            parent, wargs, n);
  /*
   * Register callback function for when the user moves the
   * scrollbar slider.
   */
  XtAddCallback(w,XmNvalueChangedCallback,slider_moved,data);
  XtAddCallback(w, XmNdragCallback, slider_moved, data);
  return (w);
}

void slider_moved(w, data, call_data)
     Widget        w;
     flight_data  *data;
     XmScaleCallbackStruct *call_data;
{
  /*
   * Set the member of the flight_data corresponding to
   * the slider that invoked this callback.
   */
  if(w == angle_ctl)  
     data->angle = call_data->value;
  else if(w == speed_ctl)  
     data->speed = call_data->value;
  else if(w == temp_ctl)   
     data->altitude = (float) call_data->value / 10.0;
  /*
   * Replace the previous contents of the property
   * with the new data.
   */
  XChangeProperty(XtDisplay(w),
                  DefaultRootWindow(XtDisplay(w)), 
                  FLIGHT_DATA, FLIGHT_DATA_TYPE,
                  32, PropModeReplace,
                  (unsigned char *) data,
                  sizeof(flight_data) / 4);
}

