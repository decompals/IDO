
/*
 *		xlines.c
 *
 *    'xlines.c' is an example of mixing Motif widgets and menus, and Xlib
 *  drawing ability.  A simple popup menu has been attached to the Right
 *  mouse button, which contains options for how many lines are drawn, and
 *  a quit button for easy exit.  The rest of the program merely draws 
 *  colored lines into a pixmap so that when the window receives an expose
 *  type event, it copies the pixmap to the screen.  The lines that are
 *  drawn are randomly generated.
 *
 *	                                                Ivan Hajadi
 *                                                 Dave Shreiner
 *                                                 20 July 1991
 */

#include <math.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>

#define MAX_COLORS	7
#define MAX_LINES		30

/* ---  Define Global Variables  --- */

struct {
	GC		base;
	GC		clear;
	GC		color[MAX_COLORS + 1];
}	gc;

int		num_lines, lines_changed;
Pixmap	pix;
Widget	root, canvas;
void draw_lines(Widget canvas);
void force_redraw();


void main(int argc, char **argv)
{
	char				prog[80];
	Widget				menu, create_menu();
	XtAppContext			app;
	XtCallbackProc 			expose_callback();
	void				post_menu();

	strcpy(prog, argv[0]);
	root = XtAppInitialize(&app, "XLines", NULL, 0, &argc, argv, NULL, NULL, 0);

	canvas = XmCreateDrawingArea(root, "canvas", NULL, 0);
	XtManageChild(canvas);

	menu = create_menu(canvas, prog);
	XtAddEventHandler(canvas, ButtonPressMask, FALSE, post_menu, menu);

	XtAddCallback(canvas, XmNexposeCallback, 
			    (XtCallbackProc)expose_callback, NULL);

	XtRealizeWidget(root);
	XtAppMainLoop(app);
}

/* --- Create a popup menu --- */

Widget create_menu(Widget parent, char *menu_title)
{
	int					i;
	char					name[64];
	Arg					arg;
	Widget				menu, tmp, *pb;
	XmString				xmstr;
	XtCallbackProc		set_num_lines(), quit();

	static int button[] = { 10, 25, 50, 100 };

	menu = XmCreatePopupMenu(parent, "menu", NULL, 0);
	
	xmstr = XmStringCreateSimple(menu_title);
	XtSetArg(arg, XmNlabelString, xmstr);
	tmp = XmCreateLabel(menu, "menuTitle", &arg, 1);
	XtManageChild(tmp);
	XmStringFree(xmstr);

	tmp = XmCreateSeparator(menu, "menuSeparator", NULL, 0);
	XtManageChild(tmp);

	pb = (Widget *) calloc(XtNumber(button), sizeof(Widget));

	for (i = 0; i < XtNumber(button); i++) {
		sprintf(name, "%d Lines", button[i]);
		xmstr = XmStringCreateSimple(name);
		XtSetArg(arg, XmNlabelString, xmstr);
		sprintf(name, "%dlinesButton", button[i]);
		pb[i] = XmCreatePushButton(menu, name, &arg, 1);
		XtAddCallback(pb[i], XmNactivateCallback, 
		             (XtCallbackProc)set_num_lines, button[i]);
		XmStringFree(xmstr);
	}
	XtManageChildren(pb, XtNumber(button));
	free(pb);

	tmp = XmCreatePushButton(menu, "Quit", NULL, 0);
	XtAddCallback(tmp, XmNactivateCallback, (XtCallbackProc)quit, NULL);
	XtManageChild(tmp);

	return(menu);
}


void post_menu(Widget canvas, Widget menu, XButtonPressedEvent *event)
{
	XmMenuPosition(menu, event);
	XtManageChild(menu);
}


XtCallbackProc expose_callback(Widget canvas, XtPointer junk,
	XmDrawingAreaCallbackStruct *cb)
{
	static int			inited = FALSE;
	Display				*display;
	Window				window;
	XExposeEvent			*event;
	XtCallbackProc			resize_callback();
	void				initialize();

	if (!inited) {
		initialize(canvas, event);
		draw_lines(canvas);
		XtAddCallback(canvas, XmNresizeCallback, 
			     (XtCallbackProc)resize_callback, NULL);
		inited = TRUE;
	}

	event = (XExposeEvent *) cb->event;

	display = XtDisplay(canvas);
	window = XtWindow(canvas);

	XCopyArea(display, pix, window, gc.base, event->x, event->y, event->width,
		event->height, event->x, event->y);
}


XtCallbackProc resize_callback(Widget canvas, XtPointer junk,
	XmDrawingAreaCallbackStruct *cb)
{
	int					depth;
	Arg					args[3];
	Dimension			width, height;
	Display				*display;
	Window				window;
	XEvent				event;
	
	XtSetArg(args[0], XmNwidth, &width);
	XtSetArg(args[1], XmNheight, &height);
	XtSetArg(args[2], XmNdepth, &depth);
	XtGetValues(canvas, args, 3);

	display = XtDisplay(canvas);
	window = XtWindow(canvas);

	XFreePixmap(display, pix);

	pix = XCreatePixmap(display, window, width, height, depth);
	XFillRectangle(display, pix, gc.clear, 0, 0, width, height);
	draw_lines(canvas);

	force_redraw();
}


XtCallbackProc quit(Widget canvas, XtPointer junk, XtPointer cb)
{
	XtDestroyWidget(root);
	exit();
}


XtCallbackProc set_num_lines(Widget button, int number, XtPointer cb)
{
	XtUnmanageChild(XtParent(button));
	num_lines = number;
	lines_changed = TRUE;
	draw_lines(canvas);
	force_redraw();
}


void initialize(Widget canvas)
{
	int				i, n, depth, screen;
	Arg				args[5];
	Colormap			cmap;
	Display			*display;
	Dimension		width, height;
	XColor			rgb;
	XGCValues		gcv;
	Pixel				foreground, background;
	Window			window;

	static char		*color[MAX_COLORS] = { "red", "yellow", "cyan", "blue", 
		"green", "purple", "white" };

	display = XtDisplay(canvas);
	window = XtWindow(canvas);
	screen = DefaultScreen(display);

	n = 0;
	XtSetArg(args[n], XmNdepth, &depth); n++;
	XtSetArg(args[n], XmNforeground, &foreground); n++;
	XtSetArg(args[n], XmNbackground, &background); n++;
	XtSetArg(args[n], XmNwidth, &width); n++;
	XtSetArg(args[n], XmNheight, &height); n++;
	XtGetValues(canvas, args, n);

	/* --- Create the Graphics Contexts (GC's) for drawing the lines --- */

	gcv.foreground = foreground;
	gcv.background = background;

	gc.base = gc.color[0] = XtGetGC(canvas, GCForeground | GCBackground, &gcv);

	gcv.foreground = background;

	gc.clear = XtGetGC(canvas, GCForeground | GCBackground, &gcv);

	if (depth > 1) {
		cmap = DefaultColormap(display, screen);
		for (i = 0; i < MAX_COLORS; i++) {
           if (!XParseColor(display, cmap, color[i], &rgb))
              gc.color[i + 1] = gc.base;
           else
              if (!XAllocColor(display, cmap, &rgb))
                 gc.color[i + 1] = gc.base;
              else {
                 gcv.foreground = rgb.pixel;
                 gc.color[i + 1] = XtGetGC(canvas, GCForeground, &gcv);
              }
		}
	}

	/* --- Create a pixmap for drawing the lines into --- */
	pix = XCreatePixmap(display, window, width, height, depth);
	XFillRectangle(display, pix, gc.clear, 0, 0, width, height);

	num_lines = 50;
	lines_changed = FALSE;
}


void draw_lines(Widget canvas)
{
	int				i, x1, x2, y1, y2;
	Arg				args[2];
	Dimension		width, height;
	Display			*display;

	display = XtDisplay(canvas);
	
	XtSetArg(args[0], XmNwidth, &width);
	XtSetArg(args[1], XmNheight, &height);
	XtGetValues(canvas, args, 2);

	if (lines_changed) {
		XFillRectangle(display, pix, gc.clear, 0, 0, width, height);
		lines_changed = FALSE;
	}

	for (i = 0; i < num_lines; i++) {
		x1 = width * drand48();
		y1 = height * drand48();
		x2 = width * drand48();
		y2 = height * drand48();
		XDrawLine(display, pix, gc.color[i % (MAX_COLORS + 1)], x1, y1, x2, y2);
	}
}

/* --- A function to trick the XmDrawingArea widget into redrawing --- */

void force_redraw()
{
	Arg				args[2];
	Display			*display;
	Dimension		width, height;
	Window			window;
	XEvent			event;

	XtSetArg(args[0], XmNwidth, &width);
	XtSetArg(args[1], XmNheight, &height);
	XtGetValues(canvas, args, 2);
	
	display = XtDisplay(canvas);
	window = XtWindow(canvas); 

	event.type = Expose;
	event.xexpose.display = display;
	event.xexpose.window = window;
	event.xexpose.x = 0;
	event.xexpose.y = 0;
	event.xexpose.width = width;
	event.xexpose.height = height;
	event.xexpose.count = 0;

	XSendEvent(display, window, ExposureMask, TRUE, &event);
}
