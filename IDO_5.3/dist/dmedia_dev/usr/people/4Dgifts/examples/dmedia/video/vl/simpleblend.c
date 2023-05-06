/*
 * File: 	simpleblend.c
 *
 * Usage:	simpleblend.c 	[-f] [-d] [-n devicenum] [-t title] 
 *				[-v videosource] [-I] [-h]
 *			  	 
 *
 * Description:	Simpleblend is an example of blending video with graphics. It
 * 		does not do any special setup, but it does show the output 
 *		in a graphics window and on video out. This program only
 *		runs on video hardware that has a video output port.
 *		This program will only run on the Galileo or IndyVideo board
 *
 * Functions:	SGI Video Library functions used (see other modules)
 *
 *		vlOpenVideo()
 *		vlGetDeviceList()
 *		vlGetNode()
 *		vlCreatePath()
 *		vlAddNode()
 *		vlGetDevice()
 *		vlSetupPaths()
 *		vlSelectEvents()
 *		vlRegisterHandler()
 *		vlAddCallback()
 *		vlSetControl()
 *		vlGetControl()
 *		vlMainLoop()
 *		vlBeginTransfer()
 *		vlEndTransfer()
 *		vlDestroyPath()
 *		vlCloseVideo()
 *		vlGetErrno()
 *		vlPerror()
 *		vlStrError()
 *
 */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <Xm/MwmUtil.h>

#include <dmedia/vl.h>
#include <dmedia/vl_ev1.h>

#include <gl/gl.h>
#include <gl/device.h>

/*
 *  Function prototypes
 */

void	VideoTracking(Window, int,int);
void	processXEvent(uint, void *);
void	processVLEvent(VLServer, VLEvent *, void *);
void	docleanup(void);
void    usage(void);

VLServer	vlSvr = NULL;
VLPath		vlPath = -1;
VLDevList	devlist;
VLNode		src_vid;
VLNode		src_scr;
VLNode		drn_scr;
VLNode		drn_vid;
VLNode		blend_node;	/* 'cause blending controls are here */

Display		*dpy;
Window		drnwin;
int		vin = VL_ANY;
char		*_progname;
char		*deviceName;
int		devicenum = -1;
VLDev		deviceId;
int		debug = 0;

Atom WM_DELETE_WINDOW;
Atom WM_PROTOCOLS;

int lastw = 0, lasth = 0;

#define Debug	if (debug) printf
#define fDebug	if (debug) fprintf


#define ESC_KEY 0x1b

#define USAGE \
"%s: [-f] [-n <devicenum>] [-t] [-d] [-I] [-v <videonode> ] [-b <yuv>] [-h]\n\
\t-f\tdisable forking\n\
\t-n\tdevice number to use\n\
\t-t\tset window title\n\
\t-d\tdebug mode\n\
\t-I\tprint node and path IDs for command line interface users\n\
\t-v\tvideo source node number\n\
\t-b\tflat background color (hex: YYUUVV)\n\
\t-h\tthis help message\n"

void 
usage()
{
    fprintf(stderr, USAGE, _progname);
}

/* Constrain the window's aspect ratio */
void
constrainWindowResize(Display *dpy, Window win, int max_w, int max_h)
{
    XSizeHints xsh;

    xsh.min_aspect.x = xsh.max_aspect.x = 80;
    xsh.min_aspect.y = xsh.max_aspect.y = 60;
    xsh.min_width  = max_w;
    xsh.min_height = max_h;
    xsh.max_width  = max_w;
    xsh.max_height = max_h;
    xsh.flags = PAspect|PMinSize|PMaxSize;

    XSetWMNormalHints(dpy, win, &xsh);
    XResizeWindow(dpy, win, max_w, max_h);
}

main(int argc, char **argv)
{
    char *winname = NULL;
    int opterr = 0;
    int c;
    int dev;
    int ret;
    int nofork = 0;
    int x, y;
    int print_ids = 0;
    Atom WM_PROT_SET[2];
    Atom _SGI_VIDEO;
    VLControlValue val;
    int	bg_flag = 0;
    int bg_y, bg_u, bg_v;

    _progname = argv[0];

    /*
     * Parse command line options
     * -f   disable forking
     * -n   device number
     * -d   set debugging
     * -t   set title
     * -v   use video input n
     * -I   print node and path IDs
     * -b   set flat background to yyuuvv
     * -h   display help message
     */
    while ((c = getopt(argc, argv, "t:dv:In:fb:h")) != EOF) 
    {
	switch(c) 
	{
	    case 'f':
		nofork = 1;
	    break;
	    
	    case 'n':
		devicenum = atoi(optarg);
	    break;
	    
	    case 'd':
		debug++;
	    break;
	    
	    case 't':
		winname = optarg;
	    break;
	    
	    case 'I':
		print_ids = 1;
	    break;
	    
	    case 'v':
		vin = atoi(optarg);
		Debug("video input = %d\n", vin);
	    break;

	    case 'b':
		sscanf(optarg, "%2x%2x%2x", &bg_y, &bg_u, &bg_v);
		bg_flag = 1;
	    break;
	    
	    case 'h':
		usage();
		exit(0);
	    break;
	}
    }

    if (opterr) 
    {
    	usage();
	exit(1);
    }

    /* Run in background if no-forking option not set */
    if (!nofork) 
    {
	ret = fork();
	switch (ret) 
	{
	    case 0:
	    break;
	    
	    case -1:
		fprintf(stderr, "%s: can't fork\n");
		exit(1);
	    break;
	    
	    default:
		exit(0);
	    break;
	}
    }

    /* Connect to the daemon */
    if (!(vlSvr = vlOpenVideo(""))) 
    {
	printf("%s: opening video: %s\n",_progname,vlStrError(vlGetErrno()));
	exit(1);
    }

    /* Get the list of devices the daemon supports */
    if (vlGetDeviceList(vlSvr, &devlist) < 0) 
    {
	printf("%s: getting device list: %s\n",_progname,vlStrError(vlGetErrno()));
	exit(1);
    }

    /* Make sure that the device the user requested (if any) is in the list */
    if ((devicenum >= (int)devlist.numDevices) || (devicenum < -1))
    {
	if (devlist.numDevices == 1)
	    fprintf(stderr,"%s: The device number must be 0\n",_progname);
	else
	    fprintf(stderr,"%s: The device number must be between 0 and %d\n",
		    _progname, devlist.numDevices-1);
	exit(1);
    }

    /*
     * Establish a path between the screen source and video
     * drain. Then add a video source node and a screen drain
     * node.
     */

    /* Setup drain nodes on the screen and video */
    drn_scr = vlGetNode(vlSvr, VL_DRN, VL_SCREEN, VL_ANY);
    drn_vid = vlGetNode(vlSvr, VL_DRN, VL_VIDEO, VL_ANY);
    
    /* Setup source nodes on the screen and video */
    src_scr = vlGetNode(vlSvr, VL_SRC, VL_SCREEN, VL_ANY);
    src_vid = vlGetNode(vlSvr, VL_SRC, VL_VIDEO, vin);

    blend_node = vlGetNode(vlSvr, VL_INTERNAL, VL_BLENDER, VL_ANY);

    vlPath = -1;
    /* If no device was specified... */
    if (devicenum == -1)
    {
    /* Try to create the screen to video path */
	vlPath = vlCreatePath(vlSvr, VL_ANY, src_scr, drn_vid);
    /* Add the video source and screen drain nodes */
	ret = vlAddNode(vlSvr, vlPath, src_vid);
	if (!ret)
	    ret = vlAddNode(vlSvr, vlPath, drn_scr);
	if (ret)
	{ /* Device doesn't support this path, these nodes */
	    vlDestroyPath(vlSvr, vlPath);
	    vlPath = -1;
	} 
	else 
	{ /* Path OK, get device info */
	    devicenum = vlGetDevice(vlSvr, vlPath);
	    deviceName = devlist.devices[devicenum].name;
	}
    }
    else /* User specified a device */
    {	/* Get the device info */
	deviceName = devlist.devices[devicenum].name;
	deviceId   = devlist.devices[devicenum].dev;
        /* Try to create the screen to video path */
	vlPath = vlCreatePath(vlSvr, deviceId, src_scr, drn_vid);
	/* Add the video source and screen drain nodes */
	ret = vlAddNode(vlSvr, vlPath, src_vid);
	if (!ret)
	    ret = vlAddNode(vlSvr, vlPath, drn_scr);
	if (ret)
	{  /* Device doesn't support this path, these nodes */
	    vlDestroyPath(vlSvr, vlPath);
	    vlPath = -1;
	}
    }
    if (vlAddNode(vlSvr, vlPath, blend_node))
    {
	vlPerror("Add Device Node");
	vlDestroyPath(vlSvr, vlPath);
	vlPath = -1;
    }
    if (vlPath == -1)
    { /* Couldn't create the path, quit */
	vlPerror("Path Setup");
	docleanup();
    }

     /* Print the node and path IDs for cmd line users */
    if (print_ids)
    {
	printf("SIMPLEBLEND NODE IDs:\n");
	printf("screen source = %d\n", src_scr);
	printf("video source = %d\n", src_vid);
	printf("screen drain = %d\n", drn_scr);
	printf("video drain = %d\n", drn_vid);
	printf("blend = %d\n", blend_node);
	printf("PATH ID = %d\n", vlPath);
    }
    	
    /* Set up the hardware for and define the usage of the path */
    if (vlSetupPaths(vlSvr, (VLPathList)&vlPath, 1, VL_SHARE, VL_SHARE) < 0)
    {
	vlPerror(_progname);
	exit(1);
    }

    /* Set Source window first, so one gets full resolution */
    vlGetControl(vlSvr, vlPath, src_scr, VL_SIZE, &val);
    vlSetControl(vlSvr, vlPath, src_scr, VL_SIZE, &val);

    vlGetControl(vlSvr, vlPath, drn_scr, VL_SIZE, &val);
    lastw = val.xyVal.x; lasth = val.xyVal.y;

    /* Set the keyer mode, keyer source and blend controls */
    val.intVal = VL_EV1_KEYERMODE_LUMA;
    vlSetControl(vlSvr, vlPath, blend_node, VL_EV1_KEYER_MODE, &val);
    val.intVal = src_scr;
    vlSetControl(vlSvr, vlPath, blend_node, VL_BLEND_A, &val);
    val.intVal = src_vid;
    vlSetControl(vlSvr, vlPath, blend_node, VL_BLEND_B, &val);
    if (bg_flag) {
	val.intVal = bg_y;
	vlSetControl(vlSvr, vlPath, blend_node, VL_EV1_BLEND_B_Y, &val);
	val.intVal = bg_u;
	vlSetControl(vlSvr, vlPath, blend_node, VL_EV1_BLEND_B_U, &val);
	val.intVal = bg_v;
	vlSetControl(vlSvr, vlPath, blend_node, VL_EV1_BLEND_B_V, &val);
	val.intVal = 1;
	vlSetControl(vlSvr, vlPath, blend_node, VL_EV1_BLEND_B_FLAT, &val);
    }
    val.intVal = 100;
    vlSetControl(vlSvr, vlPath, blend_node, VL_EV1_WIPE_SPEED, &val);

    /* Display the drain window */
    if (!(dpy = XOpenDisplay(""))) 
    {
	printf("%s: can't open display\n", _progname);
	exit(1);
    }

    drnwin = XCreateWindow(dpy, DefaultRootWindow(dpy), 0, lasth, lastw, lasth, 0,
			    CopyFromParent, CopyFromParent, CopyFromParent,
			    (ulong) 0, NULL);


    _SGI_VIDEO = XInternAtom(dpy, "_SGI_VIDEO", False);
    WM_DELETE_WINDOW = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
    WM_PROTOCOLS = XInternAtom(dpy, "WM_PROTOCOLS", False);
    WM_PROT_SET[0] = WM_DELETE_WINDOW;
    WM_PROT_SET[1] = _SGI_VIDEO;
    XSetWMProtocols(dpy,drnwin, WM_PROT_SET, 2);

    XStoreName(dpy, drnwin, "Video Blend Drain");
    
    /* Adjust the drain window's aspect ratio */
    constrainWindowResize(dpy, drnwin, lastw, lasth);

    /* Set the video to appear in window drnwin */
     val.intVal = drnwin;
    vlSetControl(vlSvr, vlPath, drn_scr, VL_WINDOW, &val);

    /* Get the location of the screen drain */
    vlGetControl(vlSvr, vlPath, drn_scr, VL_ORIGIN, &val);
    /* Move its window to that location */
    XMoveWindow(dpy, drnwin, val.xyVal.x, val.xyVal.y);

    XMapWindow(dpy, drnwin);
    XSelectInput(dpy, drnwin, KeyPressMask|VisibilityChangeMask
			    |ExposureMask|StructureNotifyMask);
    XSync(dpy, 0);

    /* Specify a file descriptor and pending check function for VL events */			  
    vlRegisterHandler(vlSvr, ConnectionNumber(dpy), processXEvent,
		      (VLPendingFunc)XPending, dpy);
		      
    /* Set up event handler routine as callback for all events */
    vlAddCallback(vlSvr, vlPath, VLAllEventsMask, processVLEvent, NULL);

    /* Set the VL event mask so we only get control changed events */
    vlSelectEvents(vlSvr, vlPath, VLControlChangedMask);

    /* Start the data transfer immediately (i.e. don't wait for trigger) */
    vlBeginTransfer(vlSvr, vlPath, 0, NULL);

    /* Handle event dispatching */
    vlMainLoop();
}

/*
 * VideoTracking - if the user changes the size of the window, 
 * update the video to reflect the new size and position.
 */
void
VideoTracking(Window win, int x, int y)
{
    Window dummyWin;
    VLControlValue val;
 
    /* Get X's idea of origin */
    XTranslateCoordinates(dpy, win, DefaultRootWindow(dpy),
                          0, 0,
                          &x, &y,
                          &dummyWin);

    /* Try to make vl match X */
    val.xyVal.x = x;
    val.xyVal.y = y;
    if (win == drnwin)
	vlSetControl(vlSvr, vlPath, drn_scr, VL_ORIGIN, &val);
    else
	vlSetControl(vlSvr, vlPath, src_scr, VL_ORIGIN, &val);
}

/* 
 * VLEvent processing for video to screen.
 * We only deal with control changed events.
 */
void
processVLEvent(VLServer vlSvr, VLEvent *ev, void *dummy)
{
    VLControlChangedEvent *cev = (VLControlChangedEvent *) ev;
    VLControlValue val;

    Debug("VL event.type = %d\n", ev->reason);
    switch (ev->reason)
    {   /* Ignore all but a change in the drain's location */
	case VLControlChanged:
	    if ((cev->type == VL_ORIGIN)&&(cev->node == drn_scr))
	    {
	        /* Drain moved, move window accordingly */
		vlGetControl(vlSvr, vlPath, drn_scr, VL_ORIGIN, &val);
		XMoveWindow(dpy, drnwin, val.xyVal.x, val.xyVal.y);
	    }
	break;
    }
}

/* XEvent processing for video to screen */
void
processXEvent(uint fd, void *source)
{
    int i;
    VLControlValue val;
    static int old_val = 1;

    if (source == (caddr_t)dpy) 
    {
	XEvent ev;
	
	XNextEvent(dpy, &ev);
	switch (ev.type) 
	{
	    case ClientMessage:
		if (ev.xclient.message_type == WM_PROTOCOLS)
		    if (ev.xclient.data.l[0] == WM_DELETE_WINDOW)
			docleanup();
	    break;
	    
	    case Expose:		/* These really don't effect us */
	    case GraphicsExpose:
	    case VisibilityNotify:
	    break;
		
	    case ConfigureNotify:       /* Window moved or changed size */
		VideoTracking(ev.xany.window, ev.xconfigure.x,ev.xconfigure.y);
	    break;
		
	    case KeyPress: 
	    {
		XKeyEvent *kev = (XKeyEvent *)&ev;
		KeySym keysym;
		char buf[4];

				
		XLookupString(kev, buf, 1, &keysym, 0);
		switch (buf[0]) 
		{   /* Quit */
		    case 'w':
		    case 'W':
		    val.intVal = 1 - old_val;
		    old_val = val.intVal;
		    vlSetControl(vlSvr, vlPath, blend_node, VL_EV1_WIPE, &val);
		    break;
		    
		    case 'q':
		    case 'Q':
		    case ESC_KEY:
			docleanup();
		    break;
			
		    default:
		    break;
		}
	    }
	}
    }
}

/* Dispose of the vl structures */
void
docleanup(void)
{   /* End the data transfer */
    vlEndTransfer(vlSvr, vlPath);
     
    /* Destroy the path, free it's memory */
    vlDestroyPath(vlSvr, vlPath);

    /* Disonnect from the daemon */
    vlCloseVideo(vlSvr);

    exit(0);
}
