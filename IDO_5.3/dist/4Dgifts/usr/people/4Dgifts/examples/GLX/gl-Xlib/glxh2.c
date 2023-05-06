/*
 * glxh2.c:
 *
 *     This file (companion to mix2.c) provides the helper function 
 *   "GLXCreateWindow", which does all the necessary magic to create 
 *    an X window suitable for GL drawing to take place within.  
 *    see the definition of GLXCreateWindow for a description of how 
 *    to call it.
 */

#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include	<gl/glws.h>  
#include	"glxh2.h"
#include	<stdarg.h>
#include	<stdio.h>


/*
** Internal helper routines
*/

/* extract_visual:
 *  a visual is uniquely identified by a viusal identifier descibing the 
 *  visual and screen.  this function is used to get back the 
 *  GLX_NORMAL-GLX_VISUAL and the GLX_NORMAL-GLX_COLORMAP fields out of 
 *  the configuration data for use in creating a window using these data.
 *  XGetVisualInfo() returns a list of visual structures that match the 
 *  attributes explicitly specified in the template structure.
 */
static XVisualInfo*
extract_visual(Display* D, int S, GLXconfig *conf, int buffer)
{
    XVisualInfo	template, *v;
    int n;

    template.screen = S;
    template.visualid = GLXgetvalue(conf, buffer, GLX_VISUAL);
    return XGetVisualInfo (D, VisualScreenMask|VisualIDMask, &template, &n);
}



/* set_window() 
 *  search thru the current conf GL window structure looking for the 
 *  buffer that matches the mode of GLX_WINDOW so that we can go ahead 
 *  and assign the window W, (created via XCreateWindow below in 
 *  GLXCreateWindow) to the arg element.  We do this because in order 
 *  for this stuff to work, the "arg" element of the conf structure 
 *  must have the value field set to the X window id of the window 
 *  which was created.
 */
static void
set_window(GLXconfig* conf, int buffer, Window W)
{
    int	i;

    for (i = 0; conf[i].buffer; i++)
	if (conf[i].buffer == buffer && conf[i].mode == GLX_WINDOW)
	    conf[i].arg = W;
}


/*
**  Used to get a value out of the configuration structure returned by
**  GLXgetconfig.  Scan through the GLX config structure, and, when we
**  find the proper buffer-mode combination, return the argument relevant 
**  for that buffer type.
*/
unsigned long
GLXgetvalue(GLXconfig* conf, int buffer, int mode)
{
    int	i;
    for (i = 0; conf[i].buffer; i++)
	if (conf[i].buffer == buffer && conf[i].mode == mode)
	    return conf[i].arg;
    return 0;
}


/*
 * GLXCreateWindow(dpy, parent, x, y, w, h, boderWidth, arg_list, ...)
 *
 * Return value is the filled in config structure
 *
 * Arguments are:
 *	dpy		The X "Display*" returned by XOpenDisplay
 *	parent		The parent of the newly created window,
 *			a typical value for this is
 *			RootWindow(dpy, DefaultScreen(dpy))
 *	x,y		The location of the window to be created,
 *			y coordinate is measured from the top down.
 *	w,h		size of the new window
 *	borderWidth	the X border size for this window, should probably
 *			be zero.
 *	arg_list	Arguments to fill in initial config structure
 */
GLXconfig*
GLXCreateWindow(
	Display* dpy, Window parent,
	int x, int y, int w, int h,
	int borderWidth, ...)
{
    XSetWindowAttributes	init_attr;
    XWindowAttributes	read_attr;
    GLXconfig	init_config[50], *cp;
    GLXconfig	*retconfig;
    Window	win, normal_win;
    va_list	ap;
    int		buf, screen;
    XVisualInfo	*vis;

    /*
    ** Loop through the remaining arguments, copying all triples
    ** up to a zero-triple (or just plain zero if there are no
    ** triples), into the initial config structure.
    */
    va_start(ap, borderWidth);
    for (cp = init_config; buf = va_arg(ap, int); cp++) {
	cp->buffer = buf;
	cp->mode = va_arg(ap, int);
	cp->arg = va_arg(ap, int);
    }
    cp->buffer = cp->mode = cp->arg = 0;

    /*
    ** Now that we have the configuration request, call GLXgetconfig to
    ** get back real configuration information.
    */
    XGetWindowAttributes(dpy, parent, &read_attr);
    screen = XScreenNumberOfScreen(read_attr.screen);
    if ((retconfig = GLXgetconfig(dpy, screen, init_config)) ==  0) {
	fprintf(stderr, "Hardware doesn't support that window type\n");
	exit(1);
    }

    /*
    ** Now we have the information needed to actually create the
    ** windows.  There is always a normal window, so we create that
    ** one first.
    **
    ** Basically we extract the GLX_NORMAL,GLX_VISUAL and the
    ** GLX_NORMAL,GLX_COLORMAP fields out of the configuration 
    *  data, and create a window using these data.
    ** When we are done we save the X ID of the new window in
    ** the configuration structure (using set_window()).  GLXlink,
    ** and the caller of GLXCreateWindow will be interested in 
    ** this value.
    ** note the explicit definition of the init_attr.colormap 
    ** element:  we aren't sure if our visual is the same as our 
    ** parent's, and we'd like to not care.  since our colormap 
    ** and visual MUST be of the same depth and class (else the
    ** BadMatch error will bite), we pass a colormap which we 
    ** know will match our visual.  If we don't do this, we 
    ** inherit from our parent and all bets are off..
    */
    vis = extract_visual(dpy, screen, retconfig, GLX_NORMAL);
    init_attr.colormap = GLXgetvalue(retconfig, GLX_NORMAL, GLX_COLORMAP);
    init_attr.border_pixel = 0;	/* Must exist, otherwise BadMatch error */
    normal_win = XCreateWindow (dpy, parent, x, y, w, h, borderWidth,
		                vis->depth, InputOutput, vis->visual,
		                CWColormap|CWBorderPixel, &init_attr);
    set_window(retconfig, GLX_NORMAL, normal_win);
    
    /*
    ** If overlay planes were requested in the configuration, and 
    ** they are available, the GLX_OVERLAY,GLX_BUFSIZE arg field in 
    ** the returned configuration will be non zero.  If this is the 
    ** case, we create another window, in the overlay planes, a child 
    ** of the normal planes window.  The size is 2K x 2K so that the 
    ** overlay window will never have to be resized, it will always 
    ** be clipped by the size of its parent.
    */
    if (GLXgetvalue(retconfig, GLX_OVERLAY, GLX_BUFSIZE)) {
	vis = extract_visual(dpy, screen, retconfig, GLX_OVERLAY);
	init_attr.colormap = GLXgetvalue(retconfig,GLX_OVERLAY,GLX_COLORMAP);
	win = XCreateWindow (dpy, normal_win, 0, 0, 2000, 2000, 0,
		             vis->depth, InputOutput, vis->visual,
		             CWColormap|CWBorderPixel, &init_attr);
	XMapWindow(dpy, win);
	set_window(retconfig, GLX_OVERLAY, win);
    }
    
    /*
    ** Do exactly the same stuff, but this time for the popup planes
    ** when we are running on a machine that has no OVERLAY planes.
    */
    if (GLXgetvalue(retconfig, GLX_POPUP, GLX_BUFSIZE)) {
	vis = extract_visual(dpy, screen, retconfig, GLX_POPUP);
	init_attr.colormap = GLXgetvalue(retconfig,GLX_POPUP,GLX_COLORMAP);
	win = XCreateWindow (dpy, normal_win, 0, 0, 2000, 2000, 0,
		             vis->depth, InputOutput, vis->visual,
		             CWColormap|CWBorderPixel, &init_attr);
	XMapWindow(dpy, win);
	set_window(retconfig, GLX_POPUP, win);
    }

    /* now do the final step:  configure the X window for GL rendering.
    ** this informs the GL that we intend to render GL into an X window.
    ** at this point, the retconfig structure contains all the 
    ** information necessary to ensure that X and GL know about each 
    ** other and will behave in a responsible manner...
    */
    if (GLXlink(dpy, retconfig) < 0) {
	fprintf(stderr, "GLXlink failed\n");
	exit(1);
    }

    /* finally do what winopen() always did:  explicitly set the current
    ** GL window to be this new mixed model window.
    */
    GLXwinset(dpy, normal_win);

    return retconfig;
}
