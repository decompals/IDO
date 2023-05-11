/**********************************************************************************
  *  xtalk.c
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:  ??
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

#include "xtalk.h"

main(argc, argv)
     int   argc;
     char *argv[];
{
  Widget toplevel, vpane, talk, listen;
  Arg    wargs[10];
  /*
   * Open display and save display and display name.
   */
  toplevel = XtInitialize(argv[0], "Xtalk", NULL, 0,
                          &argc, argv);

  my_display = XtDisplay(toplevel);
  my_displayname = XDisplayName(my_display);
  /*
   * Create a pane to hold all other widgets.
   */
  vpane = XtCreateManagedWidget("panel", 
                                xmPanedWindowWidgetClass, 
                                toplevel, NULL,0);  
  create_command_panel(vpane);
  /*
   * Create the text panes used to talk.
   */
  XtSetArg(wargs[0], XmNeditMode, XmMULTI_LINE_EDIT);
  talk = XtCreateManagedWidget("talk", 
                               xmTextWidgetClass, 
                               vpane, wargs, 1);
  XtAddEventHandler(talk, KeyPressMask | 
                          ButtonPressMask | 
                          ButtonReleaseMask | 
                          PointerMotionMask, 
                   FALSE, send_to_remote, NULL); 
  XtSetArg(wargs[0], XmNeditMode, XmMULTI_LINE_EDIT);   
  listen = XtCreateManagedWidget("listen", 
                                 xmTextWidgetClass, 
                                vpane, NULL, 0); 
  XtAddEventHandler(listen, KeyPressMask, FALSE, 
                    warn_wrong_pane, NULL);    
  XtAddEventHandler(listen, NoEventMask, TRUE, 
                    client_message_handler, NULL);    
  XtRealizeWidget(toplevel);
  /*
   * Store the listen window ID in a public place.
   */
  register_talker_window(listen);
  XtMainLoop();
}

create_command_panel(parent)
   Widget parent;
{
 Widget  command, quit;  
 Arg     wargs[3];
 int     n;
 /*
  * Create a row widget to hold the command buttons.
  */
 command = XtCreateManagedWidget("command", 
                                  xmRowColumnWidgetClass, 
                                  parent, NULL, 0);

 quit = xs_create_quit_button (command);
 XtAddCallback(quit, XmNactivateCallback, quit_callback,NULL);
 /*
  * Create the buttons.
  */
 connect_button=XtCreateManagedWidget("connect",
                                      xmPushButtonWidgetClass,
                                      command, NULL, 0);
 XtAddCallback(connect_button, XmNactivateCallback,
               connect_callback, NULL);  
 XtSetArg(wargs[0], XtNsensitive, FALSE);
 disconnect_button =
   XtCreateManagedWidget("disconnect",xmPushButtonWidgetClass,
                         command, wargs, 1);
 XtAddCallback(disconnect_button, XmNactivateCallback,
               disconnect_callback, NULL);
 XtSetArg(wargs[0], XtNsensitive, FALSE);
 accept_button= XtCreateManagedWidget("accept", 
                                      xmPushButtonWidgetClass,
                                      command, wargs, 1);
 XtAddCallback(accept_button, XmNactivateCallback,
                accept_callback, NULL);  
 /* 
  * Create a text field in which the user can 
  * enter new machine names.
  */
 name_field = XtCreateManagedWidget("name", xmTextWidgetClass,
                                    command, NULL, 0);
 /*
  * Create the message area.
  */
 msg_field = XtCreateManagedWidget("messages", 
                                    xmLabelWidgetClass, 
                                    command, NULL, 0);
 xs_wprintf(msg_field, "No Current Connection");  
}

register_talker_window(w)
     Widget w;
{
 Window   window = XtWindow(w);
 Display *dpy    = XtDisplay(w);
 /*
  * Intern the atoms used for communication.
  */
 XTALK_WINDOW       = XInternAtom(dpy, XtNtalkWindow, 0);
 CONNECTION_REQUEST = XInternAtom(dpy,XtNconnectionRequest,0);
 CONNECTION_ACCEPT  = XInternAtom(dpy, XtNconnectionAccept,0);
 DISCONNECT_NOTIFY  = XInternAtom(dpy, XtNdisconnect, 0);
  /*
   * Store the listen window ID on our root window.
   */
  XChangeProperty(dpy, DefaultRootWindow(dpy),
                  XTALK_WINDOW, XA_WINDOW,
                  32, PropModeReplace,
                  &window, 1); 
} 

void connect_callback(w, client_data, call_data)
   Widget   w;
   caddr_t  client_data, call_data;
{  
  int        type, format, nitems, left, fail;
  Window    *retdata;
  Arg        wargs[2];
  char       *msg;
  Atom        REMOTE_XTALK_WINDOW;
  /*
   * Get the name of the display to connect to.
   */
  strcpy(othermachine, XmTextGetString(name_field));
  /*
   * Make sure the string isn't empty, so we don't connect
  * to ourselves.
   */
  if(strlen(othermachine) > 0){
    xs_wprintf(msg_field, "%s", "Trying To Open Connection");
    /*
     * Attempt to open the remote display.
     */
    if((remote_display = XOpenDisplay(othermachine)) == NULL){
        xs_wprintf(msg_field, "%s", "Connection Failed");
        return;
    }
    /*
     *  Get the REMOTE property containing THEIR listen ID.
     */
    REMOTE_XTALK_WINDOW  = 
          XInternAtom(remote_display, XtNtalkWindow, 0);
    if(XGetWindowProperty(remote_display, 
                          DefaultRootWindow(remote_display),
                          REMOTE_XTALK_WINDOW,		      
                          0, 4, FALSE, XA_WINDOW,
                          &type, &format, &nitems, &left,
                          &retdata) == Success &&
         type == XA_WINDOW){
      remote_talker_window = *retdata;
      /*
       *  If all went well, request a connection.
       */
      xs_wprintf(msg_field, "Waiting for a response");
      connection_accepted = FALSE;
      xs_send_message(remote_display, remote_talker_window,
                      XtNconnectionRequest, my_displayname);
     }
    /*
     *  If something went wrong, disconnect.
     */
    else
      disconnect_callback(disconnect_button, NULL, NULL);
  }
}

void client_message_handler(w, client_data, event)
     Widget          w;
     caddr_t         client_data;
     XEvent         *event;
{
  Arg wargs[10];
  if(event->type != ClientMessage) return;
  if(event->xclient.message_type == CONNECTION_REQUEST){
   /*
    * Notify the user of the incoming request and
    * enable the "accept" button.
    */
    XBell(XtDisplay(w), 0);
    strcpy(othermachine, event->xclient.data.b);
    xs_wprintf(msg_field, "Connection Request from: %s", 
	               othermachine);
    XtSetSensitive(accept_button, TRUE);
  }
  else
    if(event->xclient.message_type == CONNECTION_ACCEPT){
      /*
       * Notify the user that the connection has
       * been accepted. Enable the "disconnect" button 
       * and disable the "connect" button.
       */
      XBell(XtDisplay(w), 0);
      connection_accepted = TRUE;
      strcpy(othermachine, event->xclient.data.b);
      xs_wprintf(msg_field, "Connected to %s", othermachine);
      XtSetSensitive(connect_button, FALSE);
      XtSetSensitive(disconnect_button, TRUE);
    }
  else
    if(event->xclient.message_type == DISCONNECT_NOTIFY){
      /*
       * Close the remote display and reset 
       * all command buttons to their initial state.
       */
      XBell(XtDisplay(w), 0);
      if(remote_display)
          	XCloseDisplay(remote_display);
      remote_display = NULL;
      connection_accepted = FALSE;
      othermachine[0] = '\0';
      xs_wprintf(msg_field, "%s", "Disconnected");

      XtSetSensitive(connect_button, TRUE);
      XtSetSensitive(disconnect_button, FALSE);
    }
}

void accept_callback(w, client_data, call_data)
     Widget    w;
     caddr_t   client_data, call_data;
{  
  int      type, format, nitems, left, fail;
  Window  *retdata;
  Atom     REMOTE_XTALK_WINDOW;
  Arg      wargs[10];
  /*
   * Make sure there really is another machine.
   */
  if(strlen(othermachine) > 0 ){
   /*
    * Attempt to open the remote display.
    */
   if((remote_display = XOpenDisplay(othermachine)) == NULL){
       xs_wprintf(msg_field, "%s", "Connection Failed");
       return;
   }
   /*
    *  Get the window ID of the remote xtalk program
    */
   REMOTE_XTALK_WINDOW  = 
                XInternAtom(remote_display, XtNtalkWindow, 0);
   if(XGetWindowProperty(remote_display,
                         DefaultRootWindow(remote_display),
                         REMOTE_XTALK_WINDOW,		      
                         0, 4, FALSE, XA_WINDOW,
                         &type, &format, &nitems, &left,
                         &retdata) == Success &&
           type ==  XA_WINDOW) {
     connection_accepted = TRUE;
     remote_talker_window = *retdata;
    /*
     * Notify the remote xtalk that we accept the connection.
     */
     connection_accepted = TRUE;
     xs_send_message(remote_display, remote_talker_window,
                     XtNconnectionAccept, my_displayname);
     xs_wprintf(msg_field, "Connected to %s", othermachine);

     XtSetSensitive(accept_button, FALSE);
     XtSetSensitive(connect_button, FALSE);
     XtSetSensitive(disconnect_button, TRUE);
    }
    else
      disconnect_callback(disconnect_button, NULL, NULL);
  }
}

void send_to_remote(w, client_data, event)
   Widget     w;
   caddr_t    client_data;
   XEvent    *event;
{  
  /*
   * Make sure that we have a valid connection 
   * before sending the event.
   */
  if(remote_display && remote_talker_window && 
       connection_accepted){
    event->xany.display = remote_display;
    event->xany.window  = remote_talker_window;
    XSendEvent(remote_display, remote_talker_window, 
               TRUE, XtAllEvents, event);
    XFlush(remote_display);
  }
}

void warn_wrong_pane(w, client_data, event)
   Widget     w;
   caddr_t    client_data;
   XEvent     *event;
{  
  /*
   * Just beep if the user types into the wrong pane.
   */
  if (!event->xany.send_event)
     XBell(XtDisplay(w), 0);
}

void disconnect_callback(w, client_data, call_data)
        Widget          w;
        caddr_t         client_data, call_data;
{  
  Arg wargs[10];
  /*
   * Send a disconnect notice and close the display.
   */
  if(remote_display){
    connection_accepted = FALSE;
    xs_send_message(remote_display, remote_talker_window,
                    XtNdisconnect, my_displayname);
    XCloseDisplay(remote_display);
    xs_wprintf(msg_field, "%s", "Disconnected");
    othermachine[0] = '\0';
    remote_display = NULL;
    XtSetSensitive(connect_button, TRUE);
    XtSetSensitive(disconnect_button, FALSE);
  }
}

void quit_callback(w, client_data, call_data)
        Widget          w;
        caddr_t         client_data, call_data;
{ 
  Display *dpy = XtDisplay(w); 
  /* 
   * Inform the remote connection that we are shutting down.
   */
  if(remote_display && remote_talker_window){
    connection_accepted = FALSE;
    xs_send_message(remote_display, remote_talker_window,
                    XtNdisconnect, my_displayname);
  }
   /*
    * Clean up.
    */
   XDeleteProperty(dpy, DefaultRootWindow(dpy), XTALK_WINDOW);
}

