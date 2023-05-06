/**********************************************************************************
  * select.c: Demo a main loop that uses select() to do 
  *           background processing
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:     143-144
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
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <sys/time.h>
#include <signal.h>
#include <errno.h>

/*
 * Define callbacks.
 */
void arm_button(w, client_data, call_data)
   Widget  w;
   void   *client_data;
   void   *call_data;
{
  printf("Button armed\n");
}

void activate_button(w, client_data, call_data)
   Widget  w;
   void   *client_data;
   void   *call_data;
{
  printf("Button activated\n");
}

main(argc, argv)
    int   argc;
    char *argv[];
{
  Widget toplevel, button;
   
  toplevel = XtInitialize(argv[0], "Select", NULL, 0, 
			  &argc, argv);
  /*
   * Create the pushbutton widget.
   */
  button = XtCreateManagedWidget("button", 
				 xmPushButtonWidgetClass, 
				 toplevel, NULL, 0);
  /*
   * Add arm and activate callbacks.
   */
  XtAddCallback(button, XmNarmCallback, arm_button, NULL);
  XtAddCallback(button, XmNactivateCallback, activate_button, NULL);

  XtRealizeWidget(toplevel);

 while(TRUE){
  if(XtPending()){
     XEvent event;
     XtNextEvent(&event);
     XtDispatchEvent (&event);
  }
  else{
    struct timeval timeout;
    int readfds = 0;
    int maxfds = 1 + ConnectionNumber(XtDisplay(toplevel));
    timeout.tv_sec = 1;
    timeout.tv_usec = 0; 
    /* Do something else for a while */
    printf("doing something in the background\n");
    readfds = 1 << ConnectionNumber(XtDisplay(toplevel));
    if (select(maxfds, &readfds, NULL, NULL, &timeout) == -1){
      if (EINTR != errno)
          exit(1);
    }
  }
}
}




