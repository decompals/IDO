/*
 * File: 	videoin.c
 *
 * Usage:	videoin [-t title] [-v input] [-n devicenum] [-f] [-d] [-b]
 *			[-I] [-z n/d] [-p x y] [-h]
 *
 * Description:	Videoin sends continuous live video to the screen.
 * 		If the video to screen path is unavailable, it uses the
 *		alternate path of video to memory to screen.
 *
 * Functions:	SGI Video Library functions used
 *
 *		vlOpenVideo()
 *		vlGetDeviceList()
 *		vlGetNode()
 *		vlCreatePath()
 *		vlGetDevice()
 *		vlSetupPaths()
 *		vlRegisterHandler()
 *		vlAddCallback()
 *		vlSelectEvents()
 *		vlNextEvent()
 *		vlSetControl()
 *		vlGetControl()
 *		vlCreateBuffer()
 *		vlRegisterBuffer()
 *		vlBeginTransfer()
 *		vlMainLoop()
 *		vlEndTransfer()
 *		vlDeregisterBuffer()
 *		vlDestroyBuffer()
 *		vlDestroyPath()
 *		vlCloseVideo()
 *		vlGetActiveRegion()
 *		vlGetLatestValid()
 *		vlPutFree() 
 *		vlPerror()
 *		vlStrError()
 */

#include <sys/gfx.h>
#include <sys/ng1.h>
#include <sys/fcntl.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <Xm/MwmUtil.h>

#include <dmedia/vl.h>

#include <gl/gl.h>
#include <gl/device.h>

/*
 *  Function prototypes
 */

void	video2screen(char *);
void	VideoTracking(int,int,int,int);
void	processEvent(uint, void *);
void	processM2SEvent(VLServer, VLEvent *, void *);
void	ProcessGlEvent(int, void *);
void	ProcessVLEvents();
void	docleanup(int);
void	video2memory2screen(char *);
void	video2gfx(char *);
void	UpdateTiming();
void	OpenWindow(char *);
void	GrabField(int, int);

void    doErrorExit(char *s);
void	noborders(Display *, Window);
char *		packing_name (int packtype);
void	SetVLFormat(VLNode node);
void	SetVLPacking(VLNode drn, int default_packing);
void	SetVLRate(VLNode drn);
void	SetVLZoom(VLNode drn);
void	setVLFormat(char *format);
char *	lastdataPtr = 0;



#define checkSetControlReturn(ret) \
	if (ret != VLSuccess && vlErrno != VLValueOutOfRange) \
		doErrorExit("vlSetControl");

#define VL_PACKING_INVALID	-1

/*
 *  Global variables
 */
 
VLServer    	vlSvr = NULL;
VLPath	    	vlPath = -1;
VLDevList   	devlist;
VLDev		deviceId;
VLBuffer    	rb = NULL;
VLNode      	src;
VLNode      	drn_scr;
VLNode      	drn_gfx;
VLNode      	drn_mem;
Display	    	*dpy;
Window		vwin;
int	    	vflag = 0;
int	    	vin = 0;
char		*_progname;
char		*deviceName;
int	    	devicenum = -1;
int	    	debug = 0;
int	    	pix_size;	/* assume rgba pixels */
int 		init_position = False;
int		win_border = True;
int 		nofork = 0;
int 		init_x = 0;
int 		init_y = 0;
int	    	xsize = 0, ysize = 0;
int		timing = 0;
int 		lastw = 0, lasth = 0;
int		defaultw, defaulth;
int 		zoomw, zoomh;
int		zoom_num = 0;
int		zoom_denom = 0;
int		rate_num = 0;
int		rate_denom = 0;
int	    	scrn_width;
int	    	scrn_height;
int		print_ids = 0;
int		packing = VL_PACKING_INVALID;
int		mypixmode;
int		vl_format = -1;
int		analognode = 1;
int		displayWhenAvailable = 0;
int		bufframes = 3;
int		buffer_noadvise = 0;
int		F1_is_first;

struct vlformats {
    int		formattype;
    int		analognode;
    char *	formatname;
} vlformats[] = {
    VL_FORMAT_COMPOSITE,		1, "COMPOSITE",
    VL_FORMAT_SVIDEO,			1, "SVIDEO",
    VL_FORMAT_RGB,			1, "RGB",
    VL_FORMAT_BETACAM,			1, "BETACAM",
    VL_FORMAT_MII,			1, "MII",
    VL_FORMAT_SMPTE_YUV,		1, "SMPTE_YUV",
    VL_FORMAT_DIGITAL_COMPOSITE,	0, "DIGITAL_COMPOSITE",
    VL_FORMAT_DIGITAL_COMPONENT,	0, "DIGITAL_COMPONENT",
    VL_FORMAT_DIGITAL_COMPONENT_SERIAL,	0, "DIGITAL_COMPONENT_SERIAL",
    VL_FORMAT_DIGITAL_COMPONENT_DUAL,	0, "DIGITAL_COMPONENT_DUAL",
    VL_FORMAT_DIGITAL_COMPONENT_DUAL_SERIAL, 0,"DIGITAL_COMPONENT_DUAL_SERIAL",
    VL_FORMAT_DIGITAL_INDYCAM,		0, "DIGITAL_INDYCAM",
    -1,					0, ""
};

Atom WM_DELETE_WINDOW;
Atom WM_PROTOCOLS;

#define Debug	if (debug) printf

char *usage = 
"%s options:\n"
    "\t-t\twindow title\n"
    "\t-v\tvideo source (depends on hardware)\n"
    "\t-n\tdevice number\n"
    "\t-f\tforking disabled\n"
    "\t-F fmt\tselect input format (depends on hardware)\n"
    "\t-d\tdebug messages\n"
    "\t-b\tno window borders\n"
    "\t-B #\tuse # ring buffers\n"
    "\t-A\tdisplay when source is available\n"
    "\t-I\tprint node and path IDs\n"
    "\t-z n/d\tinitial zoom ratio\n"
    "\t-r f/s\tset rate = frames per second\n"
    "\t-p x y\twindow position (must be last option if specified)\n"
    "\t-8\tdisplay using 8 bit pixels (depends on hardware)\n"
    "\t-m\tdisplay monochrome (depends on hardware)\n"
    "\t-h\tthis help message\n";

/* 
 * Set the window property telling motif-compatible window
 * managers (e.g., 4Dwm) to not put any border on this window.
 */
 
void
noborders(Display *display, Window win)
{
    MwmHints mwmhints;
    Atom MOTIF_WM_HINTS; 
	
    mwmhints.flags = MWM_HINTS_DECORATIONS;
    mwmhints.decorations = 0 /* MWM_DECOR_BORDER */;
    MOTIF_WM_HINTS = XInternAtom(display, "_MOTIF_WM_HINTS", False);
    XChangeProperty(display, win, MOTIF_WM_HINTS, MOTIF_WM_HINTS,
    		    sizeof(long)*8, PropModeReplace,
    		    (unsigned char*) &mwmhints,
    		    sizeof(mwmhints) / sizeof(long));
}


/*    
 * Get the default window size and current zoom ratio.
 * Set the window to the default size.
 * This is called when the window is created and each time the timing
 * (NTSC, PAL) changes.
 */
 
void
getDefaultWindowSize(Display *dpy, Window win, int max_w, int max_h)
{
    VLControlValue val;
    int w, h;

    /* Get default window size */
    if (!max_w && !max_h)
    {
	if (vlGetControl(vlSvr, vlPath, src, VL_SIZE, &val))
	    doErrorExit("get size");
	w = val.xyVal.x;
	h = val.xyVal.y;
    }
    else
    {
	w = max_w;
	h = max_h;
    }

    defaultw = w;
    defaulth = h;

    /* Get current zoom ratio */
    if (vlGetControl(vlSvr, vlPath, drn_scr, VL_ZOOM, &val))
	doErrorExit("get zoom");
    zoomw = defaultw * val.fractVal.numerator / val.fractVal.denominator;
    zoomh = defaulth * val.fractVal.numerator / val.fractVal.denominator;
 }

char *
packing_name (int packtype)
{
    switch (packtype) 
    {
	case VL_PACKING_RGB_8:
	    return "RGB";

        case VL_PACKING_RGBA_8:
	    return "RGBA";
	
	case VL_PACKING_RBG_323:   
	    return "Starter Video RGB8";
	
	case VL_PACKING_RGB_332_P:	
	    return "RGB332";
	
	case VL_PACKING_Y_8_P: 
	    return "8 Bit Greyscale";
	
	default:		
	    return "???";
    }
}
#ifdef TESTPATTERN
void
draw_test_pattern(void)
{
    extern void drawTestPattern(void);

    Debug("Drawing test pattern\n");
    drawTestPattern();
    pixmode(PM_TTOB, 1);
    pixmode(PM_SIZE, mypixmode);
}
#endif

main(int argc, char **argv)
{
    char *ptr, *winname = NULL;
    int opterr = 0;
    int c;
    int ret;
    

    /* get basename of argv */
    if ((ptr = strrchr(*argv, '/')) != NULL) _progname = ++ptr;
    else _progname = *argv;

    /*
     * parse command line options
     */
    while ((c = getopt(argc, argv, "At:dv:n:fF:p:bB:Ihz:r:8m")) != EOF) 
    {
	switch(c) 
	{
	    case 'A':
	      displayWhenAvailable = 1;
	      break;

	    case 'f':
		nofork = 1;
	    break;
    
	    case 'F':
	    {
		struct vlformats *vlf;

		for (vlf = vlformats; vlf->formattype != -1; vlf++)
		    if (strcasecmp(optarg, vlf->formatname) == 0) {
			vl_format = vlf->formattype;
			analognode = vlf->analognode;
			break;
		    }

		if (vlf->formattype == -1)
		    fprintf(stderr, "Unknown format: %s\n", optarg);
	    }
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
    
	    case 'v':
		vin = atoi(optarg);
		vflag = 1;
		Debug("Video input = %d\n", vin);
	    break;
    
	    case 'p':
		init_x = atoi(argv[optind-1]);
		init_y = atoi(argv[optind]);
		if ((init_y < 0) || (init_x < 0))
		{
		    fprintf(stderr,
			    "%s: x and y coordinates must be non-negative.\n",
			    _progname);
		    opterr = 1;
		}
		init_position = 1;
		Debug("x = %d, y = %d\n", init_x,init_y);
	    break;
			
	    case 'b':
		if (win_border)
		    win_border = 0;
#ifdef DEBUG
		else
		    /* Performance Enhancement Check:
		     *
		     * A 2nd 'b' option indicates _do not_ send the
		     * vlBufferAdvise that we do not access the data in
		     * the buffer from within this program.
		     *
		     * When we _do_ send this advice, it aids the buffer
		     * manager in speeding up DMA to and from the buffer
		     * when there is _no user access_ to the data.  This is
		     * because the buffer remains uncached and does not need
		     * to have the cache flushed before or after the DMA
		     * operations.
		     *
		     * Therefore, specifying this option will increase
		     * the system overhead during capture.  But this does
		     * ferret out other problems which is why I put it in
		     * here.
		     *
		     * Because of the (DEBUG) nature of the option, we won't
		     * actually show it on the usage message.
		     */
		    buffer_noadvise++;
#endif
	    break;
			
	    case 'B':
		bufframes = atoi(optarg);
	    break;
		
	    case 'I':
		print_ids = 1;
	    break;
		
	    case 'z':
		if (sscanf(optarg,"%d/%d", &zoom_num, &zoom_denom) != 2 ||
				!zoom_num || !zoom_denom)
		{
		    fprintf(stderr, "%s: ERROR: zoom format <num>/<denom>",
				    _progname);
		    exit(1);
		}
	    break;

	    case 'r':
		if (sscanf(optarg,"%d/%d", &rate_num, &rate_denom) != 2 ||
				!rate_num || !rate_denom)
		{
		    fprintf(stderr, "%s: ERROR: rate format <frames>/<seconds>",
				    _progname);
		    exit(1);
		}
	    break;

	    case '8':
		packing = VL_PACKING_RGB_8;
		break;

	    case 'm':
		packing = VL_PACKING_Y_8_P;
		break;

	    case 'h':
		fprintf(stderr, usage, _progname);
		exit(0);
	    break;
			
	    default:
		opterr = 1;
	    break;
	}
    }
    
    if (opterr) 
    {
	fprintf(stderr, usage, _progname);
	exit(1);
    }

    if (!nofork) 
    {
	ret = fork();
	switch (ret) 
	{
	    case 0:
	    break;
		
	    case -1:
		fprintf(stderr, "%s: can't fork\n", _progname);
		exit(1);
	    break;
    
	    default:
		exit(0);
	    break;
	}
    }

    if (!winname)
	winname = strdup("video input");

    /* Connect to the daemon */
    if (!(vlSvr = vlOpenVideo(""))) 
    {
	fprintf(stderr, "%s: error opening video.\n", _progname);
	exit(1);
    }
    /* Get the list of devices the daemon supports */
    if (vlGetDeviceList(vlSvr, &devlist) < 0) 
    {
	fprintf(stderr, "%s: getting device list: %s\n", _progname,
		vlStrError(vlErrno));
	exit(1);
    }
	
    /* Make sure that the device the user requested (if any) is in the list */
    if ((devicenum >= (int)devlist.numDevices) || (devicenum < -1))
    {
	if (devlist.numDevices == 1)
	    fprintf(stderr, "%s: The device number must be 0\n", _progname);
	else
	    fprintf(stderr,"%s: The device number must be between 0 and %d\n",
		_progname, devlist.numDevices-1);
	exit(1);
    }

    /*
     * First try to establish a path between the video source
     * and the screen directly. if vlCreatePath fails (-1),
     * try to create a path from the video source to main
     * memory to the screen.
     */

    /* Setup drain nodes on the screen and memory */
    drn_scr = vlGetNode(vlSvr, VL_DRN, VL_SCREEN, VL_ANY);
    drn_gfx = vlGetNode(vlSvr,VL_DRN,VL_GFX,VL_ANY);
    drn_mem = vlGetNode(vlSvr, VL_DRN, VL_MEM, VL_ANY);

    /* Use any video input if the user didn't specify one */
    if (vflag == 0) {
	if (vl_format != -1 && strcmp(devlist.devices[0].name, "vino") == 0) {
	    /*
	     * This is not too terribly exact - it works for vino.
	     * How can we, in VL, get the node associated with a
	     * particular VL_FORMAT?
	     */
	    src = vlGetNode(vlSvr, VL_SRC, VL_VIDEO, analognode);
	}
	else
	    src = vlGetNode(vlSvr, VL_SRC, VL_VIDEO, VL_ANY);
    }
    else
        src = vlGetNode(vlSvr, VL_SRC, VL_VIDEO, vin);
	
    /* Print the node IDs for cmd line users */
    if (print_ids)
    {
	printf("VIDEOIN NODE IDs:\n");
	printf("screen drain = %d\n", drn_scr);
	printf("memory drain = %d\n", drn_gfx);
	printf("memory drain = %d\n", drn_mem);
	printf("video source = %d\n", src);
    }
    vlPath = -1;

    /* If no device was specified... */
    if (devicenum == -1)
    {
	/* Try to create the video to screen path */
	if ((vlPath = vlCreatePath(vlSvr, VL_ANY, src, drn_scr)) >= 0) {
	    devicenum = vlGetDevice(vlSvr, vlPath);
	    deviceName = devlist.devices[devicenum].name;
	}

	if (vlPath >= 0) {
	    if (print_ids)
		printf("VIDEO TO SCREEN PATH ID = %d\n", vlPath);
	    video2screen(winname);
	}
	else if ((vlPath = vlCreatePath(vlSvr, VL_ANY, src, drn_gfx)) >= 0) {
	    devicenum = vlGetDevice(vlSvr, vlPath);
	    deviceName = devlist.devices[devicenum].name;
	    if (vlPath >= 0) {
		if (print_ids)
		    printf("VIDEO TO GFX PATH ID = %d\n", vlPath);
		video2gfx(winname);
	    }
	}
	else if ((vlPath = vlCreatePath(vlSvr, VL_ANY, src, drn_mem)) >= 0) { 
	    devicenum = vlGetDevice(vlSvr, vlPath);
	    deviceName = devlist.devices[devicenum].name;
	    if (vlPath >= 0) {
		if (print_ids)
		    printf("MEMORY TO SCREEN PATH ID = %d\n", vlPath);
		video2memory2screen(winname);
	    }
	}
    }
    else 
    {	/* User specified a device */
	deviceName = devlist.devices[devicenum].name;
	deviceId   = devlist.devices[devicenum].dev;
	/* Try to create the video to screen path */
	if ((vlPath = vlCreatePath(vlSvr, deviceId, src, drn_scr)) >= 0) {
	    if (print_ids)
		printf("VIDEO TO SCREEN PATH ID = %d\n", vlPath);
	    video2screen(winname);
	}
	/* Try to create the video to memory to GFX path */
	/* Try to create the video to memory to GFX path */
	else if ((vlPath = vlCreatePath(vlSvr, deviceId, src, drn_gfx)) >= 0) {
	    if (print_ids)
		printf("MEMORY TO SCREEN PATH ID = %d\n", vlPath);
	    video2gfx(winname);
	}
	else if ((vlPath = vlCreatePath(vlSvr, deviceId, src, drn_mem)) >= 0) {
	    if (print_ids)
		printf("MEMORY TO SCREEN PATH ID = %d\n", vlPath);
	    video2memory2screen(winname);
	}
    }
    /* If we get here, something went wrong */
    docleanup(1);
}

/*
 * Main function for video to screen path
 */
void
video2screen(char *winname)

{
    int x, y;
    VLControlValue val;
    Atom WM_PROT_SET[2];
    Atom _SGI_VIDEO;
    int ret;
    XSizeHints size_hints;
    XClassHint class_hints;

    /* Setup the hardware for and define the usage of the path */
    if (vlSetupPaths(vlSvr, (VLPathList)&vlPath, 1, VL_SHARE, VL_SHARE) < 0)
    {
	vlPerror(_progname);
	exit(1);
    }
    if (!(dpy = XOpenDisplay(""))) 
    {
	fprintf(stderr, "%s: can't open display\n", _progname);
	exit(1);
    }
    scrn_width  = DisplayWidth(dpy, DefaultScreen(dpy));
    scrn_height = DisplayHeight(dpy, DefaultScreen(dpy));

    /* set format type */
    SetVLFormat(src);

    /* set packing type (also sets display mode) */
    SetVLPacking(drn_scr, VL_PACKING_INVALID);

    /* Set the frame rate */
    SetVLRate(drn_scr);

    /* Set the initial zoom ratio */
    SetVLZoom(drn_scr);

    /*
     * find out what size the screen starts at...
     */
    vlGetControl(vlSvr, vlPath, drn_scr, VL_SIZE, &val);
    lastw = val.xyVal.x;
    lasth = val.xyVal.y;
    
    /* Get the window's origin */
    vlGetControl(vlSvr, vlPath, drn_scr, VL_ORIGIN, &val);
    x = val.xyVal.x;
    y = val.xyVal.y;

    /* If user requested initial window position */
    if (init_position)	
    {
	x = init_x;
	y = init_y;	
    }
	    
    /* Create the window */
    vwin = XCreateWindow(dpy, RootWindow(dpy, DefaultScreen(dpy)),
			 x, y, lastw, lasth,
			 0, CopyFromParent, CopyFromParent,
			 CopyFromParent, (ulong) 0, NULL);
			
    /* If user requested initial window position of 0,0 */
    if (init_position)	
    {
	size_hints.flags = USPosition | USSize;
	size_hints.x = init_x;
	size_hints.y = init_y;
	size_hints.width = lastw;
	size_hints.height = lasth;
	XSetWMNormalHints(dpy, vwin, &size_hints);
    }
	
    _SGI_VIDEO = XInternAtom(dpy, "_SGI_VIDEO", False);
    WM_DELETE_WINDOW = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
    WM_PROTOCOLS = XInternAtom(dpy, "WM_PROTOCOLS", False);
    WM_PROT_SET[0] = WM_DELETE_WINDOW;
    WM_PROT_SET[1] = _SGI_VIDEO;
    XSetWMProtocols(dpy,vwin, WM_PROT_SET, 2);

    /* set class properties for 4Dwm desktop */
    class_hints.res_name = _progname;
    class_hints.res_class = _progname;
    XSetClassHint(dpy, vwin, &class_hints);

    XStoreName(dpy, vwin, winname);
    getDefaultWindowSize(dpy, vwin, lastw, lasth);
	
    /* If the user requested no window borders kill the borders */
    if (!win_border)		
	noborders(dpy, vwin); 
	    
    /* Set the video to appear in window vwin */
    val.intVal = vwin;
    ret = vlSetControl(vlSvr, vlPath, drn_scr, VL_WINDOW, &val);
    checkSetControlReturn(ret);
    XMoveResizeWindow(dpy, vwin, x, y, lastw, lasth);

    XMapWindow(dpy, vwin);
    XSelectInput(dpy, vwin, KeyPressMask|VisibilityChangeMask
		|ExposureMask|StructureNotifyMask);
    XSync(dpy, 0);
    XSync(dpy, 0);

    /* Specify a file descriptor and pending check function for X events */
    vlRegisterHandler(vlSvr, ConnectionNumber(dpy), processEvent,
	      	(VLPendingFunc)XPending, dpy);
			  
    /* Specify a file descriptor and pending check function for VL events */			  
    vlRegisterHandler(vlSvr, vlConnectionNumber(vlSvr), processEvent,
	      	(VLPendingFunc)vlPending, vlSvr);
	
/*
 * Set up the mask for control changed events and Stream preempted events.
 * We need the latter because our path is set up VL_SHARE, and that means
 * it may disappear... if so we should exit gracefully
 */
    if (vlSelectEvents(vlSvr, vlPath, VLControlChangedMask |
				      VLStreamPreemptedMask))
	       	doErrorExit("select events");

    /* Start the data transfer immediately (i.e. don't wait for trigger) */
    if (vlBeginTransfer(vlSvr, vlPath, 0, NULL))
		doErrorExit("begin transfer");

    /* Update the window size, position per user's changes */
    VideoTracking(x,y,lastw,lasth);

    /* Handle event dispatching */
    vlMainLoop();
}

/*
 * VideoTracking - if the user changes the size of the window, 
 * update the video to reflect the new size and position.
 */
void
VideoTracking(int x, int y, int w, int h)
{
    int tmpx,tmpy;
    Window dummyWin;
    VLControlValue val;
    int bw, d;
    int ret;
 
    /* Get X's idea of origin */
    XTranslateCoordinates(dpy, vwin, RootWindow(dpy, DefaultScreen(dpy)),
	    0, 0,
	    &x, &y,
	    &dummyWin);
    /* If the window didn't move and coords are OK just return */
    if ((lastw == w) && (lasth == h)) 
    {
	val.xyVal.x = x;
	val.xyVal.y = y;
	ret = vlSetControl(vlSvr, vlPath, drn_scr, VL_ORIGIN, &val);
	checkSetControlReturn(ret);
	return;
    }

    /* Get VL's idea of our size */
    if (vlGetControl(vlSvr, vlPath, drn_scr, VL_SIZE, &val))
	    doErrorExit("get size");
    /* check again... */
    if ((val.xyVal.x == w) && (val.xyVal.y == h)) 
    {
	val.xyVal.x = x;
	val.xyVal.y = y;
	ret = vlSetControl(vlSvr, vlPath, drn_scr, VL_ORIGIN, &val);
	checkSetControlReturn(ret);
	return;
    }

    /* Try to make VL match X */
    val.xyVal.x = w;
    val.xyVal.y = h;
    ret = vlSetControl(vlSvr, vlPath, drn_scr, VL_SIZE, &val);
    checkSetControlReturn(ret);

    val.xyVal.x = x;
    val.xyVal.y = y;
    ret = vlSetControl(vlSvr, vlPath, drn_scr, VL_ORIGIN, &val);
    checkSetControlReturn(ret);

    /* Check and see how close we got */
    if (vlGetControl(vlSvr, vlPath, drn_scr, VL_SIZE, &val))
	doErrorExit("get size");

    /* If we don't match, X should win. so we'll go to the last size. */

    if (w != val.xyVal.x || h != val.xyVal.y) 
    {
	XResizeWindow(dpy, vwin, val.xyVal.x, val.xyVal.y);
	XSync(dpy, 0);
	XSync(dpy, 0);
	XGetGeometry(dpy, vwin, &dummyWin, &tmpx, &tmpy,
	    (unsigned int *) &w, (unsigned int *) &h,
	    (unsigned int *) &bw, (unsigned int *) &d);
	XSync(dpy, 0);
    }
    lastw = w;
    lasth = h;
}

/*
 * VL and X event processing for video to screen
 */
void
processEvent(uint fd, void *source)
{
    VLControlValue val;
    int ret;

    /* Handle VLEvents (in this example we only get control changed events) */
    if (source == (caddr_t)vlSvr) 
    {
	VLEvent ev;
    
	vlNextEvent(vlSvr, &ev);
	Debug("VL event.type = %d\n", ev.reason);
	switch (ev.reason) 
	{
	    case VLControlChanged: 
		{
		    VLControlChangedEvent *cev = (VLControlChangedEvent *) &ev;
    
		    switch (cev->type)
		    {
		/* If the timing (NTSC,PAL) changes get new default window size */
			case VL_TIMING:
			    getDefaultWindowSize(dpy, vwin, 0, 0);
			break;
					
		/* If the user changes the window's location... */
			case VL_ORIGIN:
			    if (vlGetControl(vlSvr,vlPath,drn_scr,VL_ORIGIN,&val))
				doErrorExit("get origin");
			    XMoveWindow(dpy, vwin, val.xyVal.x, val.xyVal.y);
			break;
						
		/* If the user changes the window's size... */
			case VL_SIZE:
			    if (vlGetControl(vlSvr,vlPath,drn_scr,VL_SIZE,&val))
				doErrorExit("get size");
			    XResizeWindow(dpy, vwin, val.xyVal.x, val.xyVal.y); 
			break;
			case VL_ZOOM:
			    getDefaultWindowSize(dpy, vwin, 0, 0);
			break;
		    }
		}		
	    break;
	    
	    case VLStreamPreempted:
		fprintf(stderr, "%s: Stream was preempted by another Program\n",
			_progname);
	        docleanup(1);

	    break;
	}
    }

    /* Handle XEvents */
    if (source == (caddr_t)dpy) 
    {
	    XEvent ev;
    
	    XNextEvent(dpy, &ev);
	    switch (ev.type) 
	    {
	    case ClientMessage:
		if (ev.xclient.message_type == WM_PROTOCOLS)
		    if (ev.xclient.data.l[0] == WM_DELETE_WINDOW)
			docleanup(0);
	    break;
	    
	    case Expose:		/* These really don't affect us */
	    case GraphicsExpose:
	    case VisibilityNotify:
	    break;
	    
	    case ConfigureNotify:
		if(init_position)	/* User requested intial window position */
		    VideoTracking(init_x, init_y, ev.xconfigure.width,ev.xconfigure.height);
		else
		    VideoTracking(ev.xconfigure.x,ev.xconfigure.y,	
		ev.xconfigure.width,ev.xconfigure.height);
	    break;
	    
	    case KeyPress: 
	    {
		XKeyEvent *kev = (XKeyEvent *)&ev;
		KeySym keysym;
		char buf[4];
		
		XLookupString(kev, buf, 1, &keysym, 0);
		switch (keysym) 
		{
		    case XK_f:
			vlGetControl(vlSvr,vlPath,drn_scr,VL_FREEZE,&val);
			if (val.boolVal)
			    val.boolVal = FALSE;
			else
			    val.boolVal = TRUE;
			vlSetControl(vlSvr,vlPath,drn_scr,VL_FREEZE,&val);
			break;
		    case XK_q:
		    case XK_Q:
		    case XK_Escape:
			    docleanup(0);
		    break;
		    
		    /* Zoom in */
		    case XK_KP_Add:
		    case XK_plus:
		    /* Get current zoom ratio */
			if (vlGetControl(vlSvr,vlPath,drn_scr,VL_ZOOM,&val))
			    doErrorExit("get zoom");
		    /* Set ratio to zoom in */
			if (val.fractVal.denominator > 1)
			    val.fractVal.denominator -= 1;
			else
			    val.fractVal.numerator += 1;
			ret = vlSetControl(vlSvr,vlPath,drn_scr,VL_ZOOM,&val);
			checkSetControlReturn(ret);
		    /* Check to see if zoom ratio is OK for this hardware */
			if (vlGetControl(vlSvr,vlPath,drn_scr,VL_ZOOM,&val))
			    doErrorExit("get zoom");
			zoomw = defaultw * val.fractVal.numerator /
				val.fractVal.denominator;
			zoomh = defaulth * val.fractVal.numerator /
				val.fractVal.denominator;
		    break;
	    
		    /* Zoom out */
		    case XK_KP_Subtract:
		    case XK_minus:
	            /* Get current zoom ratio */
			if (vlGetControl(vlSvr,vlPath,drn_scr,VL_ZOOM,&val))
			    doErrorExit("get zoom");
			if (val.fractVal.numerator > 1)
			    val.fractVal.numerator -= 1;
			else
			    val.fractVal.denominator += 1;
			ret = vlSetControl(vlSvr,vlPath,drn_scr,VL_ZOOM,&val);
			checkSetControlReturn(ret);
		    /* Check to see if zoom ratio is OK for this hardware */
			if (vlGetControl(vlSvr,vlPath,drn_scr,VL_ZOOM,&val))
			    doErrorExit("get zoom");
			zoomw = defaultw * val.fractVal.numerator /
				val.fractVal.denominator;
			zoomh = defaulth * val.fractVal.numerator /
				val.fractVal.denominator;
		    break;
		    
		    /* Pan right */
		    case XK_Left:
		    /* Get current offset */
			if (vlGetControl(vlSvr,vlPath,drn_scr,VL_OFFSET,&val))
			    doErrorExit("get offset");
			val.xyVal.x++;
			ret = vlSetControl(vlSvr,vlPath,drn_scr, VL_OFFSET, &val);
		    /* Make sure offset is OK */
			checkSetControlReturn(ret);
		    break;
		    
		    /* Pan left */
		    case XK_Right:
		    /* Get current offset */
			if (vlGetControl(vlSvr,vlPath,drn_scr,VL_OFFSET,&val))
			    doErrorExit("get offset");
			val.xyVal.x--;
			ret = vlSetControl(vlSvr,vlPath,drn_scr, VL_OFFSET, &val);
		    /* Make sure offset is OK */
			checkSetControlReturn(ret);
		    break;
		    
		    /* Pan up */
		    case XK_Down:
		    /* Get current offset */
			if (vlGetControl(vlSvr,vlPath,drn_scr,VL_OFFSET,&val))
			    doErrorExit("get offset");
			val.xyVal.y--;
			ret = vlSetControl(vlSvr,vlPath,drn_scr, VL_OFFSET, &val);
		    /* Make sure offset is OK */
			checkSetControlReturn(ret);
		    break;
		    
		    /* Pan down */
		    case XK_Up:
		    /* Get current offset */
			if (vlGetControl(vlSvr,vlPath,drn_scr,VL_OFFSET,&val))
			    doErrorExit("get offset");
			val.xyVal.y++;
			ret = vlSetControl(vlSvr,vlPath,drn_scr, VL_OFFSET, &val);
		    /* Make sure offset is OK */
			checkSetControlReturn(ret);
		    break;
		    
		    default:
		    break;
		}
	    }
	}
    }
}

/* 
 * Error reporting and program cleanup routine
 */
void
doErrorExit(char *s)
{
    char foo[80];

    sprintf(foo,"%s(%s)", _progname, s);
    vlPerror(foo);
    docleanup(1);
}

/*
 * Dispose of the VL structures
 */
void
docleanup(int ret)
{
    /* Stop the data transfer */
    vlEndTransfer(vlSvr, vlPath);

    if (rb)
    {
	/* Disassociate the ring buffer from the path */
	vlDeregisterBuffer(vlSvr, vlPath, drn_mem, rb);
	/* Destroy the ring buffer, free the memory it used */
	vlDestroyBuffer(vlSvr, rb);
    }
    /* Destroy the path, free it's memory */
    vlDestroyPath(vlSvr, vlPath);

    /* Disconnect from the daemon */
    vlCloseVideo(vlSvr);

    /*
     * XXX -gordo
     *  this needs to restore the colormap
     */

    exit(ret);
}


/*
 * Main function for video to memory to screen. Used if video
 * device does not directly support video to screen. Unlike the 
 * direct video to screen path this uses the graphics library rather
 * than XWindows to display the video. The GL uses DMA to get the
 * data from memory to screen.
 */
void
video2memory2screen(char *winname)
{
    long    win;
    long    at_top = 0;
    long    screen_ymax = 0;
    int     mask;
    int     ret;
    VLControlValue val;

    /* Setup the hardware for and define the usage of the path */
    if (vlSetupPaths(vlSvr, (VLPathList)&vlPath, 1, VL_SHARE, VL_SHARE) < 0)
    {
	vlPerror(_progname);
	exit(1);
    }

    /* set format type */
    SetVLFormat(src);
    
    /* set packing type (also sets display mode) */
    SetVLPacking(drn_mem, VL_PACKING_RGBA_8);

    /* set frame rate */
    SetVLRate(drn_mem);

    /* Set the initial zoom ratio */
    SetVLZoom(drn_mem);

    /*
     * Make sure that this size is supported. If this
     * size is not supported, the next closest size is
     * returned
     */

    if (vlGetControl(vlSvr, vlPath, drn_mem, VL_SIZE, &val))
	doErrorExit("get size");
    xsize = val.xyVal.x;
    ysize = val.xyVal.y;
    
    if (!val.xyVal.x || !val.xyVal.y)
    {
	printf("ERROR: default size = 0\n");
	docleanup(1);
    }

    /* If forking is disabled run in foreground only */
    if(nofork) 
	foreground();
    /* If the user requested no borders */
    if (!win_border)	
	noborder();
    prefsize(xsize, ysize);
    /* If the user requested initial window position */
    if (init_position)	
    {	
	screen_ymax = getgdesc(GD_YPMAX);	/* Screen y dimension in pixels */
	at_top = screen_ymax - ysize;	/* Window location to put it at top of screen */
	/* Put the window where the user requested */
	prefposition(init_x, init_x + xsize, at_top - init_y, screen_ymax - init_y);
    }
    if (winname == NULL)
	winname = "Continuous Capture";

    win = winopen(winname);
    RGBmode();
    gconfig();

    /*
     * Set up the GL to lrectwrite from top to bottom
     * (bottom to top default). This way the sense of
     * the video frame is the same as that of the
     * graphics window.
     */
    pixmode(PM_TTOB, 1);
#ifdef TESTPATTERN
    draw_test_pattern();
#endif

    val.intVal = VL_CAPTURE_INTERLEAVED;
    vlSetControl(vlSvr, vlPath, drn_mem, VL_CAP_TYPE, &val);

/*
 * Set up the mask for transfer complete, transfer failed and stream 
 * preempted events.
 * We need the last because our path is set up VL_SHARE, and that means
 * it may disappear... if so we should exit gracefully
 */
    mask = VLTransferCompleteMask |
#ifdef TESTPATTERN
	   VLSyncLostMask |
#endif
	   VLStreamPreemptedMask | VLTransferFailedMask;

    if (displayWhenAvailable)
	mask |= VLStreamAvailableMask;

    if (vlSelectEvents(vlSvr, vlPath, mask))
	doErrorExit("select events");

    /* Set processM2SEvent() as the callback for a transfer complete event */
    vlAddCallback(vlSvr, vlPath, mask, processM2SEvent, NULL);
    
    /* Select GL keyboard and window events to be handled */
    qdevice(ESCKEY);
    qdevice(QKEY);
    qdevice(WINSHUT);
    qdevice(WINQUIT);    
    qdevice(REDRAW);

    /* Tell the VL that the GL event handler will handle GL events */
    vlRegisterHandler(vlSvr, qgetfd(), (VLEventHandler)ProcessGlEvent,
		      (VLPendingFunc) qtest, (void *)win);

    /*
     * Set up a small ring buffer (two frames worth). This
     * way the vl can write to one buffer while we are dis-
     * playing the other.
     */
    rb = vlCreateBuffer(vlSvr, vlPath, drn_mem, bufframes);

    /* Tell vl that we won't be touching the buffer, so it can go faster */
    if (buffer_noadvise) {
	Debug("Buffer Advise = none\n");
    }
    else {
	Debug("Buffer Advise = No Access\n");
	if (vlBufferAdvise(rb, VL_BUFFER_ADVISE_NOACCESS))
	    doErrorExit("vlBufferAdvise");
    }

    /* Associate the ring buffer with the path */
    if (vlRegisterBuffer(vlSvr, vlPath, drn_mem, rb))
	doErrorExit("register buffer");

    /* Begin the data transfer */
    if (vlBeginTransfer(vlSvr, vlPath, 0, NULL))
	doErrorExit("begin transfer");
 	
    /* Handle event dispatching */
    vlMainLoop();
}

void
processM2SEvent(VLServer svr, VLEvent *ev, void *data)
{
    VLInfoPtr info;
    char *dataPtr;

    switch (ev->reason)
    {
	case VLTransferComplete:
	    /* Get a pointer to the most recently captured frame */
	    while (info = vlGetLatestValid(vlSvr, rb)) {

		/* Get the valid video data from that frame */
		dataPtr = vlGetActiveRegion(vlSvr, rb, info);
	    
		/* Write the data to the screen */
		lrectwrite(0, 0, xsize-1, ysize-1, (ulong *)dataPtr);
    
		/* Done with that frame, free the memory used by it */

		/* (new for 5.3 - currently implemented only gl lrectwrite)
		 * If we already have a "last" frame pointer, then the
		 * following free releases the frame just before the
		 * current one.  In any case, we'll save the current as
		 * the "last" frame.
		 */
		if (lastdataPtr)
		    vlPutFree(vlSvr, rb);

		lastdataPtr = dataPtr;
	    }
	break;
	
	case VLTransferFailed:    
	    Debug("Transfer Failed\n");
		fprintf(stderr, "%s: Memory to screen transfer failed.\n",
			_progname);
		docleanup(1);
	break;
	
	case VLStreamPreempted:  
	    Debug("Stream Preempted\n");
	    if (!displayWhenAvailable) {
		fprintf(stderr, "%s: Stream was preempted by another Program\n",
			_progname);
	        docleanup(1);
	    }

	    vlEndTransfer(vlSvr, vlPath); /* Does not appear to be necessary */

	break;

        case VLStreamAvailable:
	    /* Restart the transfer */
	    Debug("Stream Available\n");
	    vlBufferReset(vlSvr, rb);
	    vlSetupPaths(vlSvr, &vlPath, 1, VL_SHARE, VL_SHARE);
	    vlBeginTransfer(vlSvr, vlPath, 0, NULL); 
	    break;

#ifdef TESTPATTERN
	case  VLSyncLost:
	    draw_test_pattern();
	    break;
#endif

	default:
	    Debug("Unknown Event: %d\n", ev->reason);
	break;
    }
}

/* Handle graphics library events */
void
ProcessGlEvent(int fd, void *win)
{
    static short val;

    switch (qread(&val))
    {   /* Quit */
	case QKEY :
	case ESCKEY:
	    if (val == 1) /* Handle keypresses only */
	        docleanup(0);
	break;
		
	case WINSHUT:
	case WINQUIT:
	    docleanup(0);
	break;

	case REDRAW:
	    lrectwrite(0, 0, xsize-1, ysize-1, (ulong *)lastdataPtr);
	    break;

	default:
	break;
    }
}

void
video2gfx(char *winname)
{ 
    short dev, val;    

    if (vlSetupPaths(vlSvr, (VLPathList)&vlPath, 1, VL_SHARE, VL_SHARE) < 0) {
	vlPerror(_progname);
	exit(1);
    }   
    UpdateTiming();

    OpenWindow(winname);

    if (vlSelectEvents(vlSvr, vlPath, VLControlChangedMask
				    | VLStreamPreemptedMask ))
	doErrorExit("select events");

    /*
    vlBeginTransfer(vlSvr, vlPath, 0, NULL); 
    */

    /* Handle event dispatching */
    while (TRUE) {
	if (qtest()) {
	    dev = qread(&val);
	    switch (dev) {
		case ESCKEY:
		case QKEY:
		case WINSHUT:
		case WINQUIT:
		    docleanup(0);
		    break;
		default:
		    break;
	    }	    
	}
	ProcessVLEvents();
	GrabField(0, F1_is_first);
	swapbuffers();
	GrabField(1, F1_is_first);
	swapbuffers();
    }
    docleanup(0);
}

void UpdateTiming()
{
    VLControlValue  val;
    VLControlValue dominance;
    int is_525;

    int dom_ctrl = (('S' << 24) | ('I' << 16)) + 43;

      /* Get the timing on selected input node */
    if (vlGetControl(vlSvr, vlPath, src, VL_TIMING, &val) <0) {
        vlPerror("VlGetControl:TIMING");
        exit(1);
    }
    /* Set the GFX Drain to the same timing as input src */
    if (vlSetControl(vlSvr, vlPath, drn_gfx, VL_TIMING, &val) <0) {
        vlPerror("VlSetControl:TIMING");
        exit(1);
    }
    timing = val.intVal;
    if (vlGetControl(vlSvr, vlPath, drn_gfx, VL_SIZE, &val) <0) {
        vlPerror("VlGetControl");
        exit(1);
    }
    xsize = val.xyVal.x;
    ysize = val.xyVal.y;

    /*
     * Now figure out which field comes first, based upon the input timing
     * and dominance setting (if any).
     */
    if (vlGetControl(vlSvr, vlPath, src, dom_ctrl, &dominance) < 0) {
	/*
	 * For now, dom_ctrl is a Sirius device-dependent control, so if this
	 * bombs, it's probably because videoin is not running on a Sirius 
	 * board.  Assume standard field dominance in this case.
	 */
	dominance.intVal = 1; /* F1 is first (temporal order), F2 is second */
    }
    is_525 = ( (timing == VL_TIMING_525_SQ_PIX) ||
               (timing == VL_TIMING_525_CCIR601) );

    switch (dominance.intVal) {
        case 1:				/* F1 dominant */
            if (is_525) {
                F1_is_first = 0;
            } else {
                F1_is_first = 1;
            }
            break;
        case 2:				/* F2 dominant */
            if (is_525) {
                F1_is_first = 1;
            } else {
                F1_is_first = 0;
            }
            break;
	default:
	    fprintf(stderr, "%s: illegal Sirius DD dominance setting %d\n",
		_progname, dominance.intVal);
	    exit(1);
            break;
    }
}

void
OpenWindow(char *winname)
{
    prefsize(xsize, ysize);
    foreground();
    winopen(winname);
    RGBmode();
    doublebuffer();
    gconfig();
    qdevice(ESCKEY);
    qdevice(QKEY);
    qdevice(WINSHUT);
    qdevice(WINQUIT);    
}
void
ProcessVLEvents()
{
    VLEvent ev;
   
    if (vlCheckEvent(vlSvr, VLControlChangedMask|
                    VLStreamPreemptedMask, &ev) == -1) {
        return;
    }
    switch(ev.reason) {
        case VLStreamPreempted:
            docleanup(1);
            exit(0);
        break;
        case VLControlChanged:
            switch(ev.vlcontrolchanged.type) {
                case VL_TIMING:
                case VL_SIZE:
                    UpdateTiming();
                    prefsize(xsize, ysize);
                    winconstraints();
                break;
                default:
                break;
            }
        break;
        default:
        break;
    }
}
void
GrabField(int odd_field, int F1_has_first_field)
{
    int  line_shift;
    long buf[6];

    if (F1_has_first_field) {
        line_shift = 1 - odd_field;
    } else {
        line_shift = odd_field;
    }

    /* copy pixels from front to back buffer */
    readsource(SRC_FRONT);
    rectcopy(0, 0, (xsize - 1), (ysize - 1), 0, 0);

    /* The following code is to get GL to skip lines
     * in order to interleave fields.
     */
    buf[0] = 6970;
    buf[1] = 1;
    glcompat(1005, (long) buf);

     /* video is upside down relative to graphics */
    pixmode(PM_TTOB,1);

    /* Copy the field from the video device to GFX subsystem */
    buf[0] = xsize;
    buf[1] = ysize / 2;
    buf[2] = 0;
    buf[3] = -line_shift;
    buf[4] = 0;
    buf[5] = 0;
    glcompat(9000, (long) buf);

    /* undo any strange settings */
    buf[0] = 6970;
    buf[1] = 0;
    glcompat(1005, (long) buf);
    pixmode(PM_TTOB,0);
}

/*****************************************************************
 *****************************************************************
 **                                                             **
 **   Common processing functions to set various VL options.    **
 **                                                             **
 *****************************************************************
 *****************************************************************/

/* Set the packing type for the video board
 *
 * Sending VL_PACKING_INVALID as "default_packing" means:
 *    don't set it unless user specified it.
 */
void
SetVLPacking(VLNode drn, int default_packing)
{
    VLControlValue val;
    int ret;
    char m[128];

    if (packing == VL_PACKING_INVALID)
    {
	if (default_packing == VL_PACKING_INVALID)
	    return;
	
	packing = default_packing;
    }

    val.intVal = packing;
    if ((ret = vlSetControl(vlSvr, vlPath, drn, VL_PACKING, &val)) < 0)
	if (debug && ret) {
	    sprintf(m, "Setting VL_PACKING to %d", packing);
	    vlPerror(m);
	}
	else
	    checkSetControlReturn(ret);

    /* Set the graphics to the same packing as the video board */
    if ((ret = vlGetControl(vlSvr, vlPath, drn, VL_PACKING, &val)) < 0)
	checkSetControlReturn(ret);

    packing = val.intVal;
    pixmode(PM_TTOB, 1);

    switch (val.intVal)
    {
	case VL_PACKING_Y_8_P:
	/*
	 * XXX -gordo
	 *  this needs to restore the colormap
	 */
	{
	    int i;
	    
	    /* Use colormap mode for greyscale */
	    cmode();			
	    for (i = 0; i < 256; i++)
		mapcolor(i, i, i, i);
	    mypixmode = 8;
	    gconfig();			/* Reconfigure graphics */
	    pix_size = 1;
	}
	break;

	case VL_PACKING_RGB_332_P:
	    mypixmode = 9;
	    pix_size = 1;
	break;

	case VL_PACKING_RBG_323:
	    mypixmode = 8;
	    pix_size = 1;
	break;

	case VL_PACKING_RGB_8:
	case VL_PACKING_RGBA_8:
	    mypixmode = 32;
	    pix_size = 4;
	break;

	default:		
	    doErrorExit("Unknown packing type set.");
    }

    pixmode(PM_SIZE, mypixmode);
    Debug("packing = %s\n", packing_name(val.intVal));
}

/* Set the frame rate, if specified by the user, otherwise leave default */
void
SetVLRate(VLNode drn)
{
    VLControlValue val;
    int ret;
    char m[128];

    if (rate_denom)
    {
	val.fractVal.numerator = rate_num;
	val.fractVal.denominator = rate_denom;
	ret = vlSetControl(vlSvr, vlPath, drn, VL_RATE, &val);
	if (debug && ret) {
	    sprintf(m, "Setting VL_RATE to %d/%d", rate_num, rate_denom);
	    vlPerror(m);
	}
    }

    ret = vlGetControl(vlSvr, vlPath, drn, VL_RATE, &val);
    if (debug && ret)
	vlPerror("Getting VL_RATE");

    Debug("rate = %d/%d\n",
	val.fractVal.numerator, val.fractVal.denominator);
}

/* Set the zoom.  If not specified by the user, then set it to 1/1. */
void
SetVLZoom(VLNode drn)
{
    VLControlValue val;
    int ret;
    char m[128];

    if (!zoom_denom)
    {
	val.fractVal.numerator = 1;
	val.fractVal.denominator = 1;
    }
    else
    {
	val.fractVal.numerator = zoom_num;
	val.fractVal.denominator = zoom_denom;
    }
    ret = vlSetControl(vlSvr, vlPath, drn, VL_ZOOM, &val);
    if (debug && ret) {
	sprintf(m, "Setting VL_ZOOM to %d/%d", zoom_num, zoom_denom);
	vlPerror(m);
    }
    ret = vlGetControl(vlSvr, vlPath, drn, VL_ZOOM, &val);
    if (debug && ret)
	vlPerror("Getting VL_ZOOM");

    Debug("zoom = %d/%d\n",
	val.fractVal.numerator, val.fractVal.denominator);
}

void
SetVLFormat(VLNode node)
{
    VLControlValue val;
    int ret;
    char m[128];

    if (vl_format != -1) {
	val.intVal = vl_format;
	ret = vlSetControl(vlSvr, vlPath, node, VL_FORMAT, &val);
	if (debug && ret) {
	    sprintf(m, "Setting VL_FORMAT to %d" , vl_format);
	    vlPerror(m);
	}
    }
    ret = vlGetControl(vlSvr, vlPath, node, VL_FORMAT, &val);
    if (debug && ret)
	vlPerror("Getting VL_FORMAT");

    Debug("format = %d\n", val.intVal);
}
