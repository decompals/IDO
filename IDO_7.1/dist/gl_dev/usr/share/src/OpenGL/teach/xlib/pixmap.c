/* compile: cc -o pixmap pixmap.c -lGL -lX11 */

#include <GL/glx.h>
#include <X11/keysym.h>
#include <stdlib.h>
#include <stdio.h>

static int attributeList[] = { GLX_RGBA, None };
static Display* dpy;
static GC xgc;
static Pixmap pixmap;
static GLXPixmap glxPixmap;
static Window win;
static int width = 300;
static int height = 300;

static void refresh(void);
static void draw_scene(void);
static void process_input(void);
static void error(const char* prog, const char* msg);

static void
refresh(void) {
	/* don't redraw; just copy pixmap to window */
	XCopyArea(dpy, pixmap, win, xgc, 0, 0, width, height, 0, 0);
	}

static void
draw_scene(void) {
    glClearColor(0.5, 0.5, 0.5, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);
    glColor3f(1.0,0.0,0.0);
    glRectf(-.5,-.5,.5,.5);
    glColor3f(0.0,1.0,0.0);
    glRectf(-.4,-.4,.4,.4);
    glColor3f(0.0,0.0,1.0);
    glRectf(-.3,-.3,.3,.3);
    glFlush();
}

static void
process_input(void) {
    XEvent event;
    Bool redraw = 0;

    do {
	char buf[31];
	KeySym keysym;

	XNextEvent(dpy, &event);
	switch(event.type) {
	case Expose:
	case ConfigureNotify:
	    redraw = 1;
	    break;
	case KeyPress:
	    (void) XLookupString(&event.xkey, buf, sizeof(buf), &keysym, NULL);
	    switch (keysym) {
	    case XK_Escape:
		exit(EXIT_SUCCESS);
	    default:
		break;
	    }
	default:
	    break;
	}
    } while (XPending(dpy));
    if (redraw) refresh();
}

static void
error(const char *prog, const char *msg) {
    fprintf(stderr, "%s: %s\n", prog, msg);
    exit(EXIT_FAILURE);
}

int
main(int argc, char **argv) {
    XVisualInfo *vi;
    XSetWindowAttributes swa;
    GLXContext cx;

    /* get a connection */
    dpy = XOpenDisplay(0);
    if (!dpy) error(argv[0], "can't open display");

    /* get an appropriate visual */
    vi = glXChooseVisual(dpy, DefaultScreen(dpy), attributeList);
    if (!vi) error(argv[0], "no suitable visual");

    /* create a GLX context */
    cx = glXCreateContext(dpy, vi, 0, GL_FALSE);

    /* create a color map */
    swa.colormap = XCreateColormap(dpy, RootWindow(dpy, vi->screen),
                                   vi->visual, AllocNone);

    /* create a window */
    swa.border_pixel = 0;
    swa.event_mask = ExposureMask | StructureNotifyMask | KeyPressMask;
    win = XCreateWindow(dpy, RootWindow(dpy, vi->screen), 0, 0, width, height,
			0, vi->depth, InputOutput, vi->visual,
			CWBorderPixel|CWColormap|CWEventMask, &swa);
    XStoreName(dpy, win, "pixmap");
    XMapWindow(dpy, win);

    /* create the pixmap */
    xgc = XCreateGC(dpy, win, 0, NULL);
    pixmap = XCreatePixmap(dpy, win, width, height, vi->depth);
    glxPixmap = glXCreateGLXPixmap(dpy, vi, pixmap);

    /* connect the context to the pixmap, and set up transforms to match */
    glXMakeCurrent(dpy, glxPixmap, cx);
    glViewport(0, 0, width, height);
    glOrtho(-1, 1, -1, 1, -1, 1);

    /* generate the image in the pixmap */
    draw_scene();
    glXWaitGL();

    /* handle events directed to the window */
    while (1) process_input();
}
