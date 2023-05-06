/* 
 * xcedit.c
 *
 *   Xcedit is a sample program to show how to write color X/Motif program
 *   using read/write cell in a Default Colormap.
 *
 * Ivan M. Hajadi, Silicon Graphics Inc., 11-Mar-90
 */


#include "xcedit.h"

main(argc, argv)
int argc;
char **argv;
{
  Widget toplevel, form, sliders;
  Widget red_slider, green_slider, blue_slider;
  Arg    wargs[10];
  int    n;
  unsigned long pixels[1];

  /* initialize the toolkit */
  progname = argv[0];
  toplevel = XtInitialize(progname, "XCedit", NULL, 0,
		 &argc, argv);

  display  = XtDisplay(toplevel);

  /* create manager widget to hold the sliders and the label */
  form = XtCreateManagedWidget("layout", xmFormWidgetClass,
			     toplevel, NULL, 0);

  /* create pop up menu */
  create_menu(form);

  /* create the sliders */
  sliders = XtCreateManagedWidget("sliderpanel", xmRowColumnWidgetClass,
				       form, NULL, 0);

  red_slider   = make_slider("red", sliders, slider_moved);
  green_slider = make_slider("green", sliders, slider_moved);
  blue_slider  = make_slider("blue", sliders, slider_moved);

  /* setup color map */

  cmap = DefaultColormap(display, DefaultScreen(display));

  /* allocate 1 read/write color cell */

  if (!XAllocColorCells(display, cmap, False, NULL, 0,  pixels, 1))
  {
     fprintf(stderr, "Cannot allocate color cell.\n");
     exit(-1);
  }

  color.pixel = pixels[0];
  color.flags = DoRed | DoGreen | DoBlue;

  /* get the RGB components to be shown in the label */

  XQueryColor(display, cmap, &color);
  sprintf(buf, "%d  %d  %d", color.red, color.green, color.blue);

  /* make a small window using label widget */
  n = 0;
  XtSetArg(wargs[n], XmNbackground, color.pixel); n++;
  XtSetArg(wargs[n], XmNlabelString, 
		     XmStringCreate(buf, XmSTRING_DEFAULT_CHARSET)); n++;
  collabel = XtCreateManagedWidget("collabel", xmLabelWidgetClass,
				   form, wargs, n);


  /* instantiate the widgets now */
  XtRealizeWidget(toplevel);

  /* create arrow cursor for the application */
  create_cursor(form);

  /* tell the X server to use cmap as a colormap for this application */
  XSetWindowColormap(display, XtWindow(toplevel), cmap);

  XtMainLoop();

}


void create_cursor(w)
Widget w;
{
  /* uses standard X cursor (see Appendix I vol 2 of O'reilly book) */
  Cursor c = XCreateFontCursor(display, XC_left_ptr);
  XDefineCursor(display, XtWindow(w), c);
  XFreeCursor(display, c);
}


Widget make_slider(name, parent, callback)
char *name; 
Widget parent;
void (*callback)();
{
  Widget slider;
  int    n;
  Arg    wargs[10];
  XtPointer client_data;

  n = 0;
  XtSetArg(wargs[n], XmNminimum, 0); n++;
  XtSetArg(wargs[n], XmNmaximum, 65535); n++;

  slider = XtCreateManagedWidget(name, xmScaleWidgetClass,
				       parent, wargs, n);

  switch (name[0]) 
  { 
    case 'r': 
      client_data = (XtPointer) "r";
      break;
    case 'g': 
      client_data = (XtPointer) "g";
      break;
    case 'b': 
      client_data = (XtPointer) "b";
      break;
    default:
      break;
  }
  XtAddCallback(slider, XmNvalueChangedCallback, callback, client_data);
  XtAddCallback(slider, XmNdragCallback, callback, client_data);

  return(slider);
}


void slider_moved(w, client_data, call_data)
Widget w;
XtPointer client_data;
XmScaleCallbackStruct *call_data;
{
  int n = 0;
  Arg wargs[10];
  char *str = (char *)client_data;

  switch (str[0]) 
  {
    case 'r':
      color.red = call_data->value;
      break;
    case 'g':
      color.green = call_data->value;
      break;
    case 'b':
      color.blue = call_data->value;
      break;
    default:
      break;
  }

  /* change the label with new value */
  sprintf(buf, "%d  %d  %d", color.red, color.green, color.blue);
  XtSetArg(wargs[n], XmNlabelString, 
		     XmStringCreate(buf, XmSTRING_DEFAULT_CHARSET)); n++;
  XtSetValues(collabel, wargs, n);

  /* update color map cell */
  XStoreColor(display, cmap, &color);
}



void create_menu(parent)
Widget parent;
{
  Widget quit;
  Widget menu = XmCreatePopupMenu(parent, "menu", NULL, 0);
  XtAddEventHandler(parent, ButtonPressMask, FALSE, 
			    (XtEventHandler)post_menu, (XtPointer) menu);

  /* menu title is the name of the program */
  XtCreateManagedWidget(progname, xmLabelGadgetClass, menu, NULL, 0);
  XtCreateManagedWidget("separator", xmSeparatorGadgetClass, menu,
				     NULL, 0);

  /* not implemented yet */
  XtCreateManagedWidget("rgb", xmPushButtonGadgetClass, menu, NULL, 0);
  XtCreateManagedWidget("cmy", xmPushButtonGadgetClass, menu, NULL, 0);
  XtCreateManagedWidget("hsv", xmPushButtonGadgetClass, menu, NULL, 0);
  XtCreateManagedWidget("hls", xmPushButtonGadgetClass, menu, NULL, 0);

  quit = XtCreateManagedWidget("quit", xmPushButtonGadgetClass, menu,
				       NULL, 0);
  XtAddCallback(quit, XmNactivateCallback, (XtCallbackProc)Quit, "quit");

  return;
}


void Quit(w, client_data, call_data)
Widget w;
char   *client_data;
caddr_t call_data;
{
  exit(0);
}


void post_menu(w, menu, event)
Widget w;
Widget menu;
XEvent *event;
{
  Arg wargs[10];
  int button;

  /* make sure it's the correct button being pressed */
  XtSetArg(wargs[0], XmNwhichButton, &button);
  XtGetValues(menu, wargs, 1);
  if (event->xbutton.button == button)
  { 
    XmMenuPosition(menu, (XButtonPressedEvent *) event);
    XtManageChild(menu);
  }
}
