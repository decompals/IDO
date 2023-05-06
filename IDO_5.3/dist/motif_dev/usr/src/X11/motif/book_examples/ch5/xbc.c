/**********************************************************************************
  * xbc.c: An X interface to bc
  *         From:
  *                   The X Window System, 
  *            Programming and Applications with Xt
  *                   OSF/Motif Edition
  *         by
  *                Douglas Young
  *              Prentice Hall, 1990
  *
  *                 Example described on pages:    157-166
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
#include <ctype.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/PanedW.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include "libXs.h"

Widget     display;
Widget     create_button();
void       quit_bc();
void       get_from_bc();
void       send_to_bc();

main(argc, argv)
  int       argc;
  char     *argv[];
{
  Widget    toplevel, panel, keyboard, qbutton;
  Arg       wargs[10];
  int       n;
  toplevel = XtInitialize(argv[0], "Xbc", NULL, 0, 
                         &argc, argv);
  /*
   * Create a vertical paned widget as a base for the 
   * rest of the calculator.
   */
  panel = XtCreateManagedWidget("panel", 
                                xmPanedWindowWidgetClass,
                                toplevel, NULL, 0);
  /*
   * Create the calculator display.
   */
 display = XtCreateManagedWidget("display",
                                 xmTextWidgetClass,
                                 panel, NULL, 0);
  /*
   * Make the keyboard, which manages 5 rows of buttons
   */
  n = 0; 
  XtSetArg(wargs[n], XmNorientation, XmHORIZONTAL); n++;
  XtSetArg(wargs[n], XmNnumColumns, 5); n++;
  XtSetArg(wargs[n], XmNadjustLast, False); n++;
  XtSetArg(wargs[n], XmNpacking, XmPACK_COLUMN); n++;
  keyboard = XtCreateManagedWidget("keyboard", 
                                   xmRowColumnWidgetClass,
                                   panel, wargs, n);
  /* 
   * Create the keyboard buttons. This order makes it 
   * look like a typical desktop calculator.
   */
  create_button("1", keyboard);
  create_button("2", keyboard);
  create_button("3", keyboard);
  create_button("+", keyboard);
  create_button("4", keyboard);
  create_button("5", keyboard);
  create_button("6", keyboard);
  create_button("-", keyboard);
  create_button("7", keyboard);
  create_button("8", keyboard);
  create_button("9", keyboard);
  create_button("*", keyboard);
  create_button("0", keyboard);
  create_button(".", keyboard);
  create_button("=", keyboard);
  create_button("/", keyboard);
  /*
   *  Create a quit button and add a callback that
   *  tells bc to exit.
   */
  qbutton = xs_create_quit_button(keyboard);
  XtAddCallback(qbutton, XmNactivateCallback, quit_bc, NULL);
  /* 
   * Add callback get_from_bc() --  invoked when input 
   * is available from stdin.
   */
  XtAddInput(fileno(stdin), XtInputReadMask, 
            get_from_bc, display);
  /* 
   * Exec the program "bc" and set up pipes 
   * between it and us.
   */
  xs_talkto("bc");

  XtRealizeWidget(toplevel);
  XtMainLoop();
}

Widget create_button(name, parent)
  char    *name;
  Widget   parent;
{
  extern void send_to_bc();
  Widget      button;  
  /*
   * Create a single button and attach an activate callback.
   */
  button = XtCreateManagedWidget(name, 
                                 xmPushButtonWidgetClass,
                                 parent, NULL, 0);
  XtAddCallback(button, XmNactivateCallback, send_to_bc,name);
  return (button);
}

void quit_bc(w, client_data, call_data)
    Widget    w;
    caddr_t   client_data, call_data;
{
   /*
    * Tell bc to quit.
    */
   fprintf(stdout, "quit\n");
}

void send_to_bc(w, buffer, call_data)
  Widget    w;
  char     *buffer;
  XmAnyCallbackStruct *call_data;
{
  static int  start_new_entry = TRUE;
  /*
   * If this is the beginning of a new operand, 
   * clear the display.
   */
  if(start_new_entry){
    reset_display();
    start_new_entry = FALSE;
  }
  switch (buffer[0]) {
  /*
   * If the user entered and '=', send bc a newline, clear
   * the display, and get ready for a new operand.
   */
  case '=':
    fprintf(stdout, "%s", XmTextGetString(display));       
    fprintf(stdout, "\n");
    reset_display();
    start_new_entry = TRUE;
    break;
  /*
   * If this is an operator, get the previous operand
   * from the display buffer, and send it to bc before 
   * sending the operand.
   */
  case '-':
  case '+':
  case '/':
  case '*':
  case '^':
    fprintf(stdout, "%s", XmTextGetString(display));  
    fprintf(stdout, "%s", buffer);
    reset_display();
    break;
  /*
   * Anything else must be a digit, so append it to the
   * display buffer.
   */
  default:
    xs_insert_string(display, buffer);
  }
  fflush(stdout);
}
void get_from_bc(w, fid, id)
  Widget       w;
  int         *fid;
  XtInputId   *id;
{
  char       buf[BUFSIZ];
  int        nbytes, i;
  /* 
   * Get all pending input and append it to the display 
   * widget. Discard lines that begin with a newline.
   */
  nbytes = read(*fid, buf, BUFSIZ);
  if (nbytes && buf[0] != '\n') {
  /*
   * Null terminate the string at the first newline,
   * or at the end of the bytes read.
   */
   for(i=0;i<nbytes;i++)
     if(buf[i] == '\n')
         buf[i] = '\0';
     buf[nbytes] = '\0'; 
     xs_insert_string(display, buf);
  }
}
reset_display()
{
  /*
   * Clear the text buffer and go to position 1.
   */
  XmTextSetString(display, "");
  XmTextSetInsertionPosition(display, 0);
}
