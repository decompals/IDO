/*
 * mix.c:
 *
 *  Example mixed model program.  To compile do:
 *
 *    cc mix.c glxhelper.c -lsphere -lgl_s -lm -lX11_s
 *
 *  This program opens up a top level X window, then creates four GL
 *  windows as children of the X window.  Each of the GL windows runs
 *  a different display mode (cmap-singlebuffer, cmap-doublebuffer,
 *  rgb-singlebuffer, and rgb-doublebuffer).  Each time a space is 
 *  pressed, the objects in the 4 GL windows rotate one step.  
 *  Pressing Escape exits the program.
 */

#include <gl/glws.h>
#include "X11/keysym.h"
#include "stdio.h"
#include "glxhelper.h"

Display* dpy;
Window top, w1, w2, w3, w4;
float angle;
float radius = 50.0;


/*
 * A little helper wrapper for GLXwinset.  It passes the global variable
 * "dpy" which contains the display, and it checks the return value.
 * This makes the call to begin GL drawing a little simpler.
 */
void Winset(Window w)
{
    int rv = GLXwinset(dpy, w);
    if (rv < 0) {
	printf("GLXWinset returned %d\n", rv);
	exit(-1);
    }
}

/*
 * This is the GL drawing stuff, skip down to "main" to see
 * how the program works.
 */

static float vs[4][3] = {
    { -1, -1, 0, },
    {  1, -1, 0, },
    {  1,  1, 0, },
    { -1,  1, 0, },
};

static float cs[4][3] = {
    { 1, 0, 0, },
    { 0, 1, 0, },
    { 0, 0, 1, },
    { 1, 1, 1, },
};

void colorIndexDraw(int c)
{
    perspective(450, 1.0, 1.0, 200.0);
    color(0);
    clear();
    translate(0, 0, -150);
    rot(angle, 'x');
    rot(angle+1, 'y');
    scale(45, 45, 0);
    color(c);
    bgnpolygon();
	v3f(vs[0]);
	v3f(vs[1]);
	v3f(vs[2]);
	v3f(vs[3]);
    endpolygon();
}

static float material[] = {
    SHININESS, 30.0,
    SPECULAR, 0.2, 0.2, 0.2,
    LMNULL,
};

static float model[] = {
    AMBIENT, 1.0, 1.0, 1.0,
    LOCALVIEWER, 0.0,
    LMNULL
};


static float light[] = {
    AMBIENT, 0.1, 0.1, 0.1,
    LCOLOR, 0.5, 1.0, 1.0,
    POSITION, 90.0, 90.0, 150.0, 0.0,
    LMNULL
};

static float lightPos[] = {
    POSITION, 0.0, 0.0, 1.0, 0.0,
    LMNULL
};

Matrix IdentityMat = {
    { 1, 0, 0, 0 },
    { 0, 1, 0, 0 },
    { 0, 0, 1, 0 },
    { 0, 0, 0, 1 },
};
Matrix RotLightMat = {
    { 1, 0, 0, 0 },
    { 0, 1, 0, 0 },
    { 0, 0, 1, 0 },
    { 0, 0, 0, 1 },
};

void rgbSetup()
{
    backface(TRUE);

    lmdef(DEFLIGHT, 1, 0, light);
    lmdef(DEFMATERIAL, 1, 0, material);
    lmdef(DEFLMODEL, 1, 0, model);

    mmode(MVIEWING);
    perspective(450, 1.0, 1.0, 200.0);


    lmbind(LIGHT0, 1);
    lmbind(LMODEL, 1);
    lmbind(MATERIAL, 1);
}

void rgbDraw()
{
    float vec[4];

    RGBcolor(0, 0, 0);
    clear();

    pushmatrix();
	loadmatrix(IdentityMat);
	rot(3, 'x');
	rot(4, 'y');
	multmatrix(RotLightMat);
	lmdef(DEFLIGHT, 1, 0, lightPos);
	getmatrix(RotLightMat);
    popmatrix();

    pushmatrix();
	translate(0, 0, -150);
	RGBcolor(255, 255, 255);
	vec[0] = 0;
	vec[1] = 0;
	vec[2] = 0;
	vec[3] = radius;
	sphdraw(vec);
    popmatrix();
}

/*
 * Called when a key is pressed, rotate a little, then draw everything.
 */

void updateAll()
{
    angle += 3.0;

    Winset(w1);
    colorIndexDraw(1);

    Winset(w2);
    colorIndexDraw(2);
    swapbuffers();

    Winset(w3);
    rgbDraw();

    Winset(w4);
    rgbDraw();
    swapbuffers();
}

void main()
{
    XSetWindowAttributes swa;
    XColor gray;

    dpy = XOpenDisplay(0);
    if (!dpy) {
	fprintf(stderr, "cannot open display\n");
	exit(-1);
    }

    /*
     * First create the top level X window.  The background will be
     * grey, and the window is only interested in key press events.
     */
    gray.red = 0x7fff; gray.green = 0x7fff; gray.blue = 0x7fff;
    XAllocColor(dpy, DefaultColormap(dpy, DefaultScreen(dpy)), &gray);
    swa.background_pixel = gray.pixel;
    swa.event_mask = KeyPressMask;
    top = XCreateWindow(dpy, RootWindow(dpy, DefaultScreen(dpy)),
			     100, 100, 230, 230,
			     0, CopyFromParent, InputOutput, CopyFromParent,
			     CWEventMask|CWBackPixel, &swa);
    XStoreName(dpy, top, "Four GL Windows");

    /*
     * Now create the four GL windows.  Each one is interested only
     * in "Expose", which is sort of like "REDRAW" in gl-speak.
     * (GLXCreateWindow is defined in glxhelper.c)
     */

    /* create the upper-left colormap-singlebuffer'd GL window */
    w1 = GLXCreateWindow(dpy, top, 10, 10, 100, 100,
			      0, GLXcolorIndexSingleBuffer);
    XSelectInput(dpy, w1, ExposureMask);

    /* create the upper-right colormap-doublebuffer'd GL window */
    w2 = GLXCreateWindow(dpy, top, 120, 10, 100, 100,
			      0, GLXcolorIndexDoubleBuffer);
    XSelectInput(dpy, w2, ExposureMask);

    /* create the lower-left rgb-singlebuffer'd GL window */
    w3 = GLXCreateWindow(dpy, top, 10, 120, 100, 100, 0, GLXrgbSingleBuffer);
    XSelectInput(dpy, w3, ExposureMask);

    /* create the lower-right rgb-doublebuffer'd GL window */
    w4 = GLXCreateWindow(dpy, top, 120, 120, 100, 100, 0, GLXrgbDoubleBuffer);
    XSelectInput(dpy, w4, ExposureMask);

    /*
     * Now that all the windows exist, cause them to appear
     */
    XMapWindow(dpy, w4);
    XMapWindow(dpy, w3);
    XMapWindow(dpy, w2);
    XMapWindow(dpy, w1);
    XMapWindow(dpy, top);

    for (;;) {

	XEvent ev;
	XNextEvent(dpy, &ev);

	switch (ev.type) {
	  case Expose:
	    /*
	     * On an expose event, redraw the affected window
	     */
	    if (ev.xexpose.window == w1) {
		Winset(w1);
		colorIndexDraw(1);

	    } else if (ev.xexpose.window == w2) {
		Winset(w2);
		colorIndexDraw(2);
		swapbuffers();

	    } else if (ev.xexpose.window == w3) {
		Winset(w3);
		rgbSetup();
		rgbDraw();

	    } else if (ev.xexpose.window == w4) {
		Winset(w4);
		rgbSetup();
		rgbDraw();
		swapbuffers();
	    }
	    break;

	  case KeyPress:
	    {
		KeySym ks;

		/*
		 * On a keypress, either rotate and re-display
		 * or exit.
		 */
		ks = XLookupKeysym(&ev.xkey, 0);
		if (ks == XK_space) {
		    updateAll();

		} else if (ks == XK_Escape) {
		    exit(0);
		}
	    }
	    break;

	}
    }
}
