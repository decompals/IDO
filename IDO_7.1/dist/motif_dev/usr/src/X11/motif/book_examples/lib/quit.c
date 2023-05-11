/*******************************************************
 * quit.c: A utility function that adds a quit button 
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  77-78
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
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>

/* 
 * Define three callbacks. Make them static - no need 
 * to make them known outside this file.
 */
static void arm_callback(w, flag, call_data)
     Widget     w;
     int       *flag;
     XmAnyCallbackStruct *call_data; 
{
 *flag = FALSE;
}
static void activate_callback(w, flag, call_data)
     Widget     w;
     int       *flag;
     XmAnyCallbackStruct *call_data; 
{
  *flag = TRUE;
}
static void disarm_callback(w, flag, call_data)
     Widget     w;
     int       *flag;
     XmAnyCallbackStruct *call_data; 
{
  if(*flag){
    XtCloseDisplay(XtDisplay(w));
    exit(0);
  }
}
/* 
 * Function to add a quit button as a child of any widget.
 */
Widget xs_create_quit_button(parent)
   Widget  parent;
{
   Widget     w;
   static int really_quit;
   w = XtCreateManagedWidget("quit", xmPushButtonWidgetClass,
                            parent, NULL, 0);
   XtAddCallback(w, XmNarmCallback, 
                 arm_callback, &really_quit);
   XtAddCallback(w, XmNdisarmCallback, 
                 disarm_callback, &really_quit);
   XtAddCallback(w, XmNactivateCallback, 
                 activate_callback, &really_quit);     
   return (w);
}
