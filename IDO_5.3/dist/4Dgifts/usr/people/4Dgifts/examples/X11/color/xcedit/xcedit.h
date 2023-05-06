/*
 * xcedit.h
 *
 * Ivan M. Hajadi, Silicon Graphics Inc., 11-Mar-91
 */


#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/SeparatoG.h>
#include <Xm/Scale.h>
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>

#include <X11/cursorfont.h>
#include <stdio.h>

char *progname;
char buf[80];
Widget collabel;
Widget red_slider, green_slider, blue_slider;

Display *display;
XColor color;

Colormap cmap;

Widget make_slider(char*, Widget, void (*)());
void slider_moved(Widget, XtPointer, XmScaleCallbackStruct*);
void Quit(Widget, char*, caddr_t);
void post_menu(Widget, Widget, XEvent*);
void create_menu(Widget);
void create_cursor(Widget);
