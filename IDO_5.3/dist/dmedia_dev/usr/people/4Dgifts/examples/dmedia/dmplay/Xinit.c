/*
 * Xinit.c: Does X / gl initialization.
 *
 * 
 * Silicon Graphics Inc., June 1994
 */

#include <signal.h>	/* sigset(), kill() */
#include <unistd.h>	/* getppid() */

#include "dmplay.h"

/*
 *Sproc include files
 */
#include <sys/types.h>
#include <sys/prctl.h>
#include <sys/wait.h>

#include <Xm/MwmUtil.h>

XtAppContext appContext;
Atom protocol, deleteWindow;

/*
 *Local Prototype definitions
 */
static void doItWindow (Widget , XEvent *, String *, Cardinal *);
static void doIt ();
static void handleEvents ();

static String fallback_resources[] = {
    "*frame*shadowType: SHADOW_IN",
    NULL
};

static XtActionsRec actionsTable[] = {
    {"doItWindow",              doItWindow},
};

static char defaultTranslations[] =
    " <Btn1Down>:      doItWindow() \n\
      <Btn2Down>:      doItWindow() \n\
      <Btn3Down>:      doItWindow() \n";

static GLXconfig glxConfig [] = {
    { GLX_NORMAL, GLX_DOUBLE, FALSE },
    { GLX_NORMAL, GLX_RGB, TRUE },
    { GLX_NORMAL, GLX_ZSIZE,  GLX_NOCONFIG },
    { 0, 0, 0 }
};

/*
 *Sproc'd processes pids'
 */
int inputthread = -1;

/*
 *X globals.
 */
static Widget glw, toplevel, form, frame;


int winWidth, winHeight;


/*
 *doIt:	Starts all the threads of the application, namely
 *			- video producer
 *			- video consumer (if display = GRAPHICS)
 *			- audio producer (only if Cosmo and display = VIDEO)
 *
 */
void
doIt ( void )
{
    playstate.advanceVideo = 1;
    playstate.advanceAudio = 1;
}	


/*
 *doItWindow:	Callback on pressing key 's' or a <Btn1Dwn>
 *
 */
void doItWindow (Widget w, XEvent *event, String *params, 
	Cardinal *num_params)
{
    doIt ();
}


/*
 *exposeWindow: called during an expose event. Clears the window on expose.
 *              On the first expose event, it calls doIt() to start the 
 *		application. It also maps the widget into a video window and 
 *		sets up the transfer of data from the camera to the 
 *		Indy/Galileo video board.
 *
 *		NOTE: The first expose event is assumed to happen immediately 
 *		with the map notify event (when the window is mapped by the X
 *		server.), which is normally the case.
 */
void exposeWindow (Widget w, caddr_t client_data, caddr_t call_data)
{
    static int first = 1;
    Window dummyWin;
    Display *dpy;
    int x,y;
    int ret;
    VLControlValue val;
    int stat;
    pid_t ppid;

    GC gc;				/* X11 graphics context */
    XGCValues	gcvalue;		/* graphics context value */

    dpy = XtDisplay(glw);

    /*Get the origin of the window in coordinate (x,y). */
    XTranslateCoordinates(dpy, XtWindow(glw),
                          RootWindow(dpy, DefaultScreen(dpy)),
                          0, 0,
                          &x, &y,
                          &dummyWin);

    if (first) {
	first = 0;

	/*
	** first thing ... clear out whatever is in this window
	*/
	gcvalue.foreground = 0;		/* better be black ... */
	gcvalue.fill_style = FillSolid;
	gc=XCreateGC(dpy, XtWindow(glw), (GCForeground|GCFillStyle), &gcvalue);
	XFillRectangle( dpy, XtWindow(glw), gc, 0, 0, val.xyVal.x, val.xyVal.y);

	if (image.display == GRAPHICS)
	    doIt ();
	else {
	    val.intVal = XtWindow(glw);
	    vlSetControl(video.svr, video.path, video.drn, VL_WINDOW, &val);
	    
	    /*
	     * Trim off top and bottom three lines
	     */
	    vlGetControl(video.svr, video.path, video.drn, VL_SIZE, &val);
	    val.xyVal.y -= 6;
	    vlSetControl(video.svr, video.path, video.drn, VL_SIZE, &val);
	    
	    vlGetControl(video.svr, video.path, video.drn, VL_OFFSET, &val);
	    val.xyVal.y -= 3;
	    vlSetControl(video.svr, video.path, video.drn, VL_OFFSET, &val);
	}
	
	/*
	 *Once the window is mapped, vl/gl  can begin transfer, thus start the
	 *video application, i.e. singleFrameDecompress() or streamDecompress()
	 */
	if ((ppid = getppid ()) == -1) {
	    perror("Unable to find video thread");
	    exit (1);
	}

	if (unblockproc (ppid) == -1) {
	    perror("Unable to start video thread");
	    exit(1);
	}
    }
}

/*
 *initWindow: initializes window.
 */
void initWindow (Widget w, caddr_t client_data, caddr_t call_data) 
{
    GLXwinset (XtDisplay (w), XtWindow (w));
    pixmode (PM_TTOB, 1);	/*Force top to bottom draw. */
}

/*
 *motionWindow: Callback to track the video display, with the displacement
 *		of the X window on the screen.
 *
 *NOTE:		This method would work only if you have one video window
 *		on the screen only.
 *
 */
void motionWindow (Widget w, caddr_t client_data, caddr_t call_data) 
{
    Window dummyWin;
    Display *dpy;
    VLControlValue val;

    dpy = XtDisplay(glw);
    XTranslateCoordinates(dpy, XtWindow(glw),
                          RootWindow(dpy, DefaultScreen(dpy)),
                          0, 0,
                          &val.xyVal.x, &val.xyVal.y,
                          &dummyWin);

    if (image.display == GRAPHICS)
	return;

    vlSetControl(video.svr, video.path, video.drn, VL_ORIGIN, &val);
}


/*
 *stopAllThreads (): Attempts to gracefully exit all the threads.
 */
void stopAllThreads () 
{
    if (video.timingActive == 1) {
	video.timingActive = 0;
	vlDestroyPath(video.svr, video.timingPath);
    }
    
    exit (0);
}

/* 
 * Initialize X toplevel widget ...
 * Parse X and User options...
 * Load resources
 */

void Xinit(int *argc, char *argv[])
{
    /* XXX what is "4DgiftsGlx" ? */
    Arg args[20];
    int n;

    n=0;
    XtSetArg(args[n], XmNmwmDecorations,
	     MWM_DECOR_ALL | MWM_DECOR_RESIZEH | MWM_DECOR_MAXIMIZE); n++;
    XtSetArg(args[n], XmNmwmFunctions,
	     MWM_FUNC_ALL | MWM_FUNC_RESIZE | MWM_FUNC_MAXIMIZE); n++;
	     
    toplevel = XtAppInitialize(&appContext, "4DgiftsGlx", 
			       (XrmOptionDescList)NULL, 0,
			       argc, (String*)argv, 
			       fallback_resources,
			       args, n);
}

/*
 *Create a window for display
 */
void Xgo( void )
{
    Arg args[20];
    XtTranslations transTable;
    int n;
    VLControlValue val;

    /*
     *to get Client message ID and message_type tag for the
     *quit/close button on the window, to exit the app.
     */
    protocol = XInternAtom (XtDisplay (toplevel), "WM_PROTOCOLS", False);
    deleteWindow = XInternAtom (XtDisplay (toplevel), 
				"WM_DELETE_WINDOW", False);


    XtAppAddActions(appContext, actionsTable, XtNumber(actionsTable));
    transTable = XtParseTranslationTable(defaultTranslations);
    
    n = 0;
    form = XmCreateForm(toplevel, "form", args, n);
    XtManageChild(form);
    
    n = 0;
    XtSetArg(args[n], XtNx, 30); n++;
    XtSetArg(args[n], XtNy, 30); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNleftOffset, 0); n++;
    XtSetArg(args[n], XmNtopOffset, 0); n++;
    XtSetArg(args[n], XmNbottomOffset, 0); n++;
    XtSetArg(args[n], XmNrightOffset, 0); n++;
    XtSetArg(args[n], XmNshadowThickness, 0); n++;
    frame = XmCreateFrame (form, "frame", args, n);
    XtManageChild (frame);
    
    n = 0;
    XtSetArg(args[n], GlxNglxConfig, glxConfig); n++;

    if (image.display != GRAPHICS) {
	/*
	 * Trim off top and bottom three lines
	 */
	vlGetControl(video.svr, video.path, video.drn, VL_SIZE, &val);
	winHeight = val.xyVal.y-6;
	winWidth = val.xyVal.x;
    } else {
	winWidth = image.width;
	if (image.interlacing == DM_IMAGE_NONINTERLACED) 
	    winHeight = 2*image.height;
	else
	    winHeight = image.height;
    }
    XtSetArg(args[n], XmNwidth, winWidth); n++;
    XtSetArg(args[n], XmNheight, winHeight); n++;
    glw = GlxCreateMDraw(frame, "glwidget", args, n);
    XtManageChild (glw);

    XtAddCallback(glw, GlxNginitCallback, (XtCallbackProc)initWindow, 0);
    XtAddCallback(glw, GlxNexposeCallback,(XtCallbackProc)exposeWindow, 0);
    
    if ((image.display == VIDEO_1_FIELD) || (image.display == VIDEO_2_FIELD)) {
	XtAddEventHandler(toplevel, SubstructureNotifyMask,
			  FALSE, 
			  (XtEventHandler)motionWindow, toplevel);
    }

    XtOverrideTranslations(glw, transTable);
    XtRealizeWidget(toplevel);

    if ((inputthread = 
	 sproc ((void (*)(void *))handleEvents, PR_SADDR | PR_BLOCK)) == -1) {
	fprintf (stderr, "Unable to sproc to handle X events\n");
	perror ("sproc");
	stopAllThreads ();
    }

    if (codec.singleFrame)
	singleFrameDecompress ();
    else
	streamDecompress();
}


static void handleEvents ()
{
    XEvent ev;
    pid_t ppid = -1;

    /*
     * Tell kernel to send us SIGHUP when our parent goes
     * away.
     */
    sigset(SIGHUP, SIG_DFL);
    prctl(PR_TERMCHILD);

    for (;;) {
        XtAppNextEvent(appContext, &ev);
        switch(ev.type) {
	  case KeyPress:
	      {
	      XKeyEvent *kev = (XKeyEvent *)&ev;
	      KeySym keysym;
	      int buf;
	      
	      XLookupString(kev, (char *)&buf, 1, &keysym, 0);
	      switch (keysym) {
		case XK_s: /* Single Step */
		  doIt ();
		  break;
		case XK_Escape:
		  if (video.timingActive == 1) {
			video.timingActive = 0;
			vlDestroyPath(video.svr, video.timingPath);
		  }
		  if ((ppid = getppid ()) == -1) {
		      perror("Unable to find video thread");
		  } else
		  if (ppid != 1)
		      kill ((pid_t)ppid, SIGTERM);
		  exit(0);
		  break;
	      }
	    }
	    break;
	    case ClientMessage:
	      if (ev.xclient.message_type == protocol && 
		  ev.xclient.data.l[0] == deleteWindow) {
		  if (video.timingActive == 1) {
			video.timingActive = 0;
			vlDestroyPath(video.svr, video.timingPath);
		  }
		  if ((ppid = getppid ()) == -1) {
		      perror("Unable to find video thread");
		  } else
		  if (ppid != 1)
		      kill ((pid_t)ppid, SIGTERM);
		  exit(0);
	      }
		  break;
	  default:
	    XtDispatchEvent (&ev); 
	    break;
        }
    }
}
