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

#include <X11/X.h>   /* X */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <Xm/MwmUtil.h>

#include <GL/glx.h>  /* OpenGL */
#include <GL/gl.h>

#include <dmedia/vl.h>  /* VL */
#include <vl/dev_sirius.h>


/* Transparent type values */
/*      None		  0 */
#define TransparentPixel      1
#define TransparentMask       2

/* layered visual info template flags */
#define VisualLayerMask		0x200
#define VisualTransparentType	0x400
#define VisualTransparentValue	0x800
#define VisualAllLayerMask	0xFFF

/* layered visual info structure */
typedef struct _XLayerVisualInfo {
   XVisualInfo vinfo;
   int layer;
   int type;
   unsigned long value;
} XLayerVisualInfo;

/* SERVER_OVERLAY_VISUALS property element */
typedef struct _OverlayInfo {
   CARD32  overlay_visual;
   CARD32  transparent_type;
   CARD32  value;
   CARD32  layer;
} OverlayInfo;

extern XLayerVisualInfo *XGetLayerVisualInfo(Display*,long,XLayerVisualInfo*,int*);
extern Status XMatchLayerVisualInfo(Display*,int,int,int,int,XLayerVisualInfo*);

/*
 *  Function prototypes
 */

void	video2screen(char *);
void	VideoTracking(int,int,int,int);
void	processEvent(uint, void *);
void	processM2SEvent(VLServer, VLEvent *, void *);
void	ProcessGlEvent(int, void *);
void	ProcessVLEvents();
Bool	ProcessXEvents();
void	docleanup(int);
void	video2memory2screen(char *);
void	video2gfx(char *);
void	UpdateTiming();
void	OpenWindow(char *);
void	GrabField(int);
void    InitGraphics(int width,int height);
void	CreateWindowAndContext(Window *win, GLXContext *ctx,int *visualAttr,char name[]);

void    doErrorExit(char *s);
void	noborders(Display *, Window);
void	SetVLFormat(VLNode node);
void	SetVLRate(VLNode drn);
void	SetVLZoom(VLNode drn);
void	setVLFormat(char *format);
char *	lastdataPtr = 0;
XVisualInfo *vi;

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
Window		window;
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
int		even_field_first;
Bool 		Quit=False;

	/* OpenGL related stuff */
GLXContext 	   ctx;
GLXVideoSourceSGIX glxVideoSource;
GLboolean 	   hasInterlace;
GLboolean 	   interlace = GL_TRUE;

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
    "\t-8\tdisplay using 8 bit pixels (depends on hardware)\n"
    "\t-m\tdisplay monochrome (depends on hardware)\n"
    "\t-h\tthis help message\n"
    "\t-p x y\twindow position (must be last option if specified)\n";

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
setZoomedWindowSize(Display *dpy, Window win)
{
    VLControlValue val;
    int ret;

    /* Get current zoom ratio */
    if (vlGetControl(vlSvr, vlPath, drn_scr, VL_ZOOM, &val))
	doErrorExit("get zoom");
    zoomw = defaultw * val.fractVal.numerator / val.fractVal.denominator;
    zoomh = defaulth * val.fractVal.numerator / val.fractVal.denominator;

    val.xyVal.x = zoomw;
    val.xyVal.y = zoomh;
    ret = vlSetControl(vlSvr, vlPath, drn_scr, VL_SIZE, &val);
    checkSetControlReturn(ret);

    ret = vlGetControl(vlSvr, vlPath, drn_scr, VL_SIZE, &val);
    checkSetControlReturn(ret);
  
    XResizeWindow(dpy, vwin, val.xyVal.x, val.xyVal.y);
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
	else
        {
        	printf("Path setup error,  Can not create a valid path with these nodes\n");
        	docleanup(1);
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
	else
	{
        	printf("Path setup error,  Can not create a valid path with this device\n");
        	docleanup(1);
        }
    }
    /*
     * If we get here, something went wrong.
     */
    printf("Internal program error, exit.\n");
    docleanup(1);
}

/*
 * Main function for video to screen path. Galileo?
 */
void
video2screen(char *winname)

{
    XSetWindowAttributes attributes;
    XLayerVisualInfo lvinfo;
    Visual *visual;
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
/*
 * Set up the mask for control changed events and Stream preempted events.
 * We need the latter because our path is set up VL_SHARE, and that means
 * it may disappear... if so we should exit gracefully
 */
    if (vlSelectEvents(vlSvr, vlPath, VLControlChangedMask |
				      VLStreamPreemptedMask))
	       	doErrorExit("select events");

    if (!(dpy = XOpenDisplay(""))) 
    {
	fprintf(stderr, "%s: can't open display\n", _progname);
	exit(1);
    }
    scrn_width  = DisplayWidth(dpy, DefaultScreen(dpy));
    scrn_height = DisplayHeight(dpy, DefaultScreen(dpy));

    if (vlGetControl(vlSvr, vlPath, src, VL_SIZE, &val))
	doErrorExit("get size");
    defaultw = val.xyVal.x;
    defaulth = val.xyVal.y;

    /* set format type */
    if ( vl_format != -1 )
	SetVLFormat(src);

    /* Set the frame rate */
    if ( rate_denom )
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
	    
    if(XMatchLayerVisualInfo(dpy,
		DefaultScreen(dpy), 8, PseudoColor, 0, &lvinfo) == 0) {
        fprintf(stderr, "No 8-bit PseudoColor visual in layer 0 found,");
        exit(1);
    } else {
	attributes.border_pixel = 0;
	attributes.background_pixel = 0;
	attributes.colormap = XCreateColormap(dpy,
			RootWindow(dpy, DefaultScreen(dpy)),
			lvinfo.vinfo.visual, AllocNone);

	vwin = XCreateWindow(dpy, RootWindow(dpy, DefaultScreen(dpy)),
			x, y, lastw, lasth, 0,
			8, InputOutput, lvinfo.vinfo.visual,
			CWBorderPixel | CWBackPixel | CWColormap, &attributes);
    }
			
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
    setZoomedWindowSize(dpy, vwin);
	
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
 * VideoTracking - if the user changes the size of the window, 
 * update the video to reflect the new size and position.
 */
void
CheckVideo(void)
{
    int tmpx,tmpy;
    Window dummyWin;
    VLControlValue val;
    int bw, d;
    int ret;
    int x,y,w,h;
 
    XTranslateCoordinates(dpy, vwin, RootWindow(dpy, DefaultScreen(dpy)),
	    0, 0,
	    &x, &y,
	    &dummyWin);
    if (vlGetControl(vlSvr,vlPath,drn_scr,VL_ORIGIN,&val))
	doErrorExit("get origin");
    if (x != val.xyVal.x || y != val.xyVal.y) {
    	XMoveWindow(dpy, vwin, val.xyVal.x, val.xyVal.y);
    }

    if (vlGetControl(vlSvr,vlPath,drn_scr,VL_SIZE, &val))
	doErrorExit("get size");
    if (lastw != val.xyVal.x || lasth != val.xyVal.y) 
    {
	w = val.xyVal.x;
	h = val.xyVal.y;
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
			    setZoomedWindowSize(dpy, vwin);
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
			    setZoomedWindowSize(dpy, vwin);
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
		int h,w;
		float zoom;
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

		    /* Resize window */
			zoom = (float) val.fractVal.numerator/
					val.fractVal.denominator;
			zoom = zoom < 2.0 ? zoom:2.0;
			h = zoom*defaulth;
			w = zoom*defaultw;
			XResizeWindow(dpy, vwin, zoom*defaultw, zoom*defaulth);
			XSync(dpy, 0);
			val.xyVal.x = w; val.xyVal.y = h;
			ret = vlSetControl(vlSvr,vlPath,drn_scr, VL_SIZE, &val);
			checkSetControlReturn(ret);

    			/* Check and see how close we got */
			CheckVideo();
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

	/* Close the X connection */
    XCloseDisplay(dpy);
    exit(ret);
}


/*
 * Main function for video to memory to screen. Used if video
 * device does not directly support video to screen. Unlike the 
 * direct video to screen path this uses the graphics library rather
 * than XWindows to display the video. 
 * The GL uses DMA to get the * data from memory to screen. 
 *   Is this Vino?
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
    				/* smallest single buffered RGBA visual */
    int visualAttr[] = {GLX_RGBA, GLX_RED_SIZE, 8, 
    			GLX_GREEN_SIZE, 8, GLX_BLUE_SIZE, 8,
                        None};
    			/* Setup the hardware for and define the usage of the path */
    if (vlSetupPaths(vlSvr, (VLPathList)&vlPath, 1, VL_SHARE, VL_SHARE) < 0)
    {
	vlPerror(_progname);
	exit(1);
    }
    SetVLFormat(src);			      /* set format type */
    val.intVal = VL_PACKING_RGBA_8;
    if ((ret = vlSetControl(vlSvr, vlPath, drn_mem, VL_PACKING, &val)) < 0)
       checkSetControlReturn(ret);
 
    /* Set the graphics to the same packing as the video board */
    if ((ret = vlGetControl(vlSvr, vlPath, drn_mem, VL_PACKING, &val)) < 0)
        checkSetControlReturn(ret);
    SetVLRate(drn_mem);			      /* set frame rate */
    SetVLZoom(drn_mem);			      /* Set the initial zoom ratio */
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
    if (winname == NULL)
	winname = "Continuous Capture";
    CreateWindowAndContext(&window,&ctx,visualAttr,winname);
    XSelectInput(dpy,window,ButtonPressMask|KeyPressMask);
    glLoadIdentity();
    glViewport(0, 0, xsize,ysize);
    glOrtho(0, xsize, 0, ysize, -1, 1);
    glRasterPos2f(0, ysize-1);
    glDrawBuffer(GL_FRONT); /* On Vino, we only use the front buffer */
    		/* video is upside down to graphics! */
    glPixelZoom(1.0, -1.0);
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
    
   			/* Handler for X events, same as sirius  */ 
    vlRegisterHandler(vlSvr, ConnectionNumber(dpy), (VLEventHandler)ProcessXEvents,
		      (VLPendingFunc) XPending, dpy);

    /*
     * Set up a small ring buffer (two frames worth). This
     * way the vl can write to one buffer while we are dis-
     * playing the other.
     */
    rb = vlCreateBuffer(vlSvr, vlPath, drn_mem, bufframes);
    if(!rb){
	doErrorExit("vlCreateBuffer");
    }

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
		glRasterPos2f(0, ysize);
				/* Get the valid video data from that frame */
		dataPtr = vlGetActiveRegion(vlSvr, rb, info);
		glDrawPixels(xsize,ysize, GL_ABGR_EXT,GL_UNSIGNED_BYTE, (char *) dataPtr);
    
		/* Done with that frame, free the memory used by it */

		/* (new for 5.3 - currently implemented only glDrawPixels)
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

/* Sirius version */
void
video2gfx(char *winname)
{ 
    short dev, val;    
    int i;
    				/* smallest single buffered RGBA visual */
    int visualAttr[] = {GLX_RGBA, GLX_RED_SIZE, 0, GLX_GREEN_SIZE, 0, 
    			GLX_BLUE_SIZE, 0, None};

    if (vlSetupPaths(vlSvr, (VLPathList)&vlPath, 1, VL_SHARE, VL_SHARE) < 0) {
	vlPerror(_progname);
	docleanup(1);
    }   
    UpdateTiming();

    CreateWindowAndContext(&window,&ctx,visualAttr,winname);
    InitGraphics(xsize,ysize);
    glxVideoSource = glXCreateGLXVideoSourceSGIX(dpy, 0, vlSvr, vlPath, VL_GFX, drn_gfx);
    if (glxVideoSource == NULL) {
        fprintf(stderr, "Can't create glxVideoSource\n");
	docleanup(0);
    }
    if (!glXMakeCurrentReadSGI(dpy, window, glxVideoSource, ctx)) {
        fprintf(stderr, "Can't make current to video\n");
	docleanup(0);
    }
    glLoadIdentity();
    glViewport(0, 0, xsize,ysize);
    glOrtho(0, xsize, 0, ysize, -1, 1);
    glDrawBuffer(GL_FRONT);
    glRasterPos2f(0, 0);
    glPixelZoom(1.0, -1.0);
    glClearColor(0.0,0.0,0.0,0.0);
    glClear(GL_COLOR_BUFFER_BIT);
    XSelectInput(dpy,window,ButtonPressMask|KeyPressMask);
    if (vlSelectEvents(vlSvr, vlPath, VLControlChangedMask
				    | VLStreamPreemptedMask ))
           doErrorExit("select events");
    vlBeginTransfer(vlSvr, vlPath, 0, NULL);  /* do we need this? */
    while(1){
	GrabField(0);
	sginap(2);
	ProcessXEvents();
	GrabField(1);
	ProcessVLEvents();
	sginap(2);
    }
    docleanup(0);
}

/* Used by Sirius */
void UpdateTiming()
{
    VLControlValue  val;
    VLControlValue dominance;
    int is_525;

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
     * Read the video source's field dominance control setting and timing,
     * then set a variable to indicate which field has the first line, so that
     * we know how to interleave fields to frames.
     */
    if (vlGetControl(vlSvr, vlPath, src, VL_SIR_FIELD_DOMINANCE, &dominance) < 0) {
        vlPerror("GetControl(VL_SIR_FIELD_DOMINANCE) on video source failed");
        exit(EXIT_FAILURE);
    }
    is_525 = ( (timing == VL_TIMING_525_SQ_PIX) ||
               (timing == VL_TIMING_525_CCIR601) );
    switch (dominance.intVal) {
        case SIR_F1_IS_DOMINANT:
            if (is_525) {
                even_field_first = 0;
            } else {
                even_field_first = 1;
            }
            break;
        case SIR_F2_IS_DOMINANT:
            if (is_525) {
                even_field_first = 1;
            } else {
                even_field_first = 0;
            }
            break;
    }
}

/* Used by Sirius */
Bool
ProcessXEvents()
{
  XEvent event;
  KeySym key;
  while(XPending(dpy)){
    XNextEvent(dpy,&event);
    switch(event.type) {
      case ButtonPress:     
        break;
      case KeyPress:
         XLookupString(&event.xkey, NULL, 0, &key, NULL);
           switch (key) {
              case XK_i:
		   interlace = !interlace;
		    glClearColor(0.0,0.0,0.0,0.0);
		    glClear(GL_COLOR_BUFFER_BIT);
		   break;
              case XK_Escape:
              case XK_q:
		docleanup(0);
              default :
                 ;  
           }
        break;
      default:
        break;
   }
 }
 return 0;
}

/* Used by Sirius */
void
ProcessVLEvents()
{
    VLEvent ev;
  while(vlPending(vlSvr)){ 
    vlNextEvent(vlSvr,&ev);
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
		   XResizeWindow(dpy, window,xsize,ysize);
    		   glLoadIdentity();
		   glViewport(0, 0, xsize,ysize);
		   glOrtho(0, xsize, 0, ysize, -1, 1);
                break;
                default:
                break;
            }
        break;
        default:
        break;
    }
  }
}
/*
 * Grab a field. A parameter of  1 = odd Field, 0 = Even Field.
 * Use the global even_field_first variable to determine how to interleave the fields.
 *  F2 = 0 => F1.
 */
void
GrabField(int F2)
{
    if (interlace) {
        if (F2) {   		/* Odd Field */
            if (even_field_first)
                glRasterPos2f(0, ysize);
            else
                glRasterPos2f(0, ysize-1);
        }
        else {   		/* Even Field */
            if (even_field_first)
                glRasterPos2f(0, ysize-1);
             else
                glRasterPos2f(0, ysize);
        }
        glEnable(GL_INTERLACE_SGIX);
        /* video is upside down relative to graphics */
        glCopyPixels(0, 0, xsize, ysize/2, GL_COLOR);
        glDisable(GL_INTERLACE_SGIX);
    } else {   /* stack up fields */
        if (F2) {
           if (even_field_first)
             glRasterPos2f(0, ysize - 1);
           else
             glRasterPos2f(0,ysize/2 - 1);
         }
         else {
           if (!even_field_first)
             glRasterPos2f(0,ysize - 1);
           else
             glRasterPos2f(0,ysize/2 - 1);
        }
        glCopyPixels(0, 0, xsize, ysize/2, GL_COLOR);
    }
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
    float zoomfactor;

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

    /* If zoom is > 1, dd layer may not adjust size for us */
    zoomfactor = (float)val.fractVal.numerator/val.fractVal.denominator;
    if (zoomfactor > 1.0) {
	vlGetControl(vlSvr, vlPath, drn, VL_SIZE, &val);
	val.xyVal.x *= zoomfactor;
	val.xyVal.y *= zoomfactor;
	vlSetControl(vlSvr, vlPath, drn, VL_SIZE, &val);
	vlGetControl(vlSvr, vlPath, drn, VL_SIZE, &val);
    }
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

static Bool layersRead;
static Atom overlayVisualsAtom;
static OverlayInfo **overlayInfoPerScreen;
static int *numOverlaysPerScreen;

XLayerVisualInfo *
XGetLayerVisualInfo(display, lvinfo_mask, lvinfo_template, nitems_return)
   Display *display;
   long lvinfo_mask;
   XLayerVisualInfo *lvinfo_template;
   int *nitems_return;
{
   XVisualInfo *vinfo;
   XLayerVisualInfo *layerInfo;
   Window root;
   Status status;
   Atom actualType;
   unsigned long sizeData, bytesLeft;
   int actualFormat, numVisuals, numScreens, count, i, j;

   vinfo = XGetVisualInfo(display, lvinfo_mask&VisualAllMask,
      &lvinfo_template->vinfo, nitems_return);
   if(vinfo == NULL)
      return NULL;
   numVisuals = *nitems_return;
   if(layersRead == False) {
      overlayVisualsAtom = XInternAtom(display, "SERVER_OVERLAY_VISUALS", True);
      if(overlayVisualsAtom != None) {
         numScreens = ScreenCount(display);
         overlayInfoPerScreen = (OverlayInfo**) malloc(numScreens*sizeof(OverlayInfo*));
         numOverlaysPerScreen = (int*) malloc(numScreens*sizeof(int));
         if(overlayInfoPerScreen != NULL && numOverlaysPerScreen != NULL) {
            for(i=0; i<numScreens; i++) {
               root = RootWindow(display, i);
               status = XGetWindowProperty(display, root, overlayVisualsAtom,
                  0L, (long)10000, False, overlayVisualsAtom, &actualType, &actualFormat,
                  &sizeData, &bytesLeft, (char**) &overlayInfoPerScreen[i]);
               if(status!=Success || actualType!=overlayVisualsAtom ||
                 actualFormat!=32 || sizeData<4)
                  numOverlaysPerScreen[i] = 0;
               else
                  numOverlaysPerScreen[i] = sizeData / (sizeof(OverlayInfo)/4);
            }
            layersRead = True;
         } else {
            if(overlayInfoPerScreen != NULL) free(overlayInfoPerScreen);
            if(numOverlaysPerScreen != NULL) free(numOverlaysPerScreen);
         }
      }
   }
   layerInfo = (XLayerVisualInfo*) malloc(numVisuals * sizeof(XLayerVisualInfo));
   if(layerInfo == NULL) {
      XFree(vinfo);
      return NULL;
   }
   count = 0;
   for(i=0; i<numVisuals; i++) {
      XVisualInfo *pVinfo;
      int screen;
      OverlayInfo *overlayInfo;

      pVinfo = &vinfo[i];
      screen = pVinfo->screen;
      overlayInfo = NULL;
      if(layersRead) {
         for(j=0; j<numOverlaysPerScreen[screen]; j++)
            if(pVinfo->visualid == overlayInfoPerScreen[screen][j].overlay_visual) {
               overlayInfo = &overlayInfoPerScreen[screen][j];
	       break;
            }
      }
      if(lvinfo_mask & VisualLayerMask)
         if(overlayInfo == NULL) {
            if(lvinfo_template->layer != 0) continue;
         } else
            if(lvinfo_template->layer != overlayInfo->layer) continue;
      if(lvinfo_mask & VisualTransparentType)
         if(overlayInfo == NULL) {
            if(lvinfo_template->type != None) continue;
         } else
            if(lvinfo_template->type != overlayInfo->transparent_type) continue;
      if(lvinfo_mask & VisualTransparentValue)
         if(overlayInfo == NULL)
            /* non-overlay visuals have no sense of TransparentValue */
            continue;
         else
            if(lvinfo_template->value != overlayInfo->value) continue;
      layerInfo[count].vinfo = *pVinfo;
      if(overlayInfo == NULL) {
         layerInfo[count].layer = 0;
         layerInfo[count].type = None;
         layerInfo[count].value = 0; /* meaningless */
      } else {
         layerInfo[count].layer = overlayInfo->layer;
         layerInfo[count].type = overlayInfo->transparent_type;
         layerInfo[count].value = overlayInfo->value;
      }
      count++;
   }
   XFree(vinfo);
   *nitems_return = count;
   if(count == 0) {
      XFree(layerInfo);
      return NULL;
   } else
      return layerInfo;
}

Status
XMatchLayerVisualInfo(display, screen, depth, class, layer, lvinfo_return)
Display *display;
int screen, depth, class, layer;
XLayerVisualInfo *lvinfo_return;
{
   XLayerVisualInfo *lvinfo;
   XLayerVisualInfo lvinfoTemplate;
   int nitems;

   lvinfoTemplate.vinfo.screen = screen;
   lvinfoTemplate.vinfo.depth = depth;
   lvinfoTemplate.vinfo.class = class;
   lvinfoTemplate.layer = layer;
   lvinfo = XGetLayerVisualInfo(display,
      VisualScreenMask|VisualDepthMask|VisualClassMask|VisualLayerMask,
      &lvinfoTemplate, &nitems);
   if(lvinfo != NULL && nitems > 0) {
      *lvinfo_return = *lvinfo;
      return 1;
   } else
      return 0;
}
/* query opengl extensions */
void
InitGraphics(int width,int height)
{ 
    int i;
    const char *glx_extensions, *gl_extensions;
   
    gl_extensions = glGetString(GL_EXTENSIONS);
    glx_extensions = glXQueryExtensionsString(dpy, DefaultScreen(dpy));
    if (gl_extensions == NULL ||
        strstr(glx_extensions, "GLX_SGI_make_current_read") == NULL) {
        fprintf(stderr, "Make_current_read extension not supported\n");
        exit(EXIT_FAILURE);
    }
    hasInterlace =
        strstr((const char *) gl_extensions, "GL_SGIX_interlace") != NULL;
    if(hasInterlace) interlace=GL_TRUE;

    if (strstr(glx_extensions, "GLX_SGIX_video_source") == NULL) {
        fprintf(stderr, "GLX video source extension\n");
        exit(EXIT_FAILURE);
    }
    glxVideoSource = glXCreateGLXVideoSourceSGIX(dpy,0,vlSvr,vlPath,VL_GFX,drn_gfx);
    if (glxVideoSource == NULL) {
        fprintf(stderr, "Can't create glxVideoSource\n");
        exit(EXIT_FAILURE);
    }
    if (!glXMakeCurrentReadSGI(dpy, window, glxVideoSource, ctx)) {
        fprintf(stderr, "Can't make current to video\n");
        exit(EXIT_FAILURE);
    }
}
Bool 
waitForMapNotify(Display *dpy, XEvent *ev, XPointer arg)
{
	    return (ev->type == MapNotify && ev->xmapping.window == *((Window *) arg));
}
XIOErrorHandler
xioerror( Display *dpy, XErrorEvent *event)
{
	    docleanup(0);
		    /*NOT REACHED*/
}

/* create and window and opengl context */
void    
CreateWindowAndContext(Window *win, GLXContext *ctx,int *visualAttr,char name[])
{
    XSizeHints hints;
    XSetWindowAttributes swa;
    Colormap cmap;
    int isRgba;
    XEvent event;
    XIOErrorHandler  oldXError;
    int i;

    hints.min_width = xsize/2;
    hints.max_width = 2*xsize;
    hints.base_width = hints.width = xsize;
    hints.min_height = ysize/2;
    hints.max_height = 2*ysize;
    hints.base_height = hints.height = ysize;
    hints.flags = USSize | PMinSize | PMaxSize;
    			/* If user requested initial window position of 0,0 */
    if (init_position)
    {
        hints.flags  |= USPosition;
        hints.x       = init_x;
        hints.y       = init_y;
    }
    /* get a connection */
    if(dpy == NULL)
       dpy = XOpenDisplay(NULL);
    if (dpy == NULL) {
        fprintf(stderr, "Can't connect to display `%s'\n",
                getenv("DISPLAY"));
        exit(EXIT_FAILURE);
    }
    oldXError = XSetIOErrorHandler (xioerror);
    
    /* get an appropriate visual */
    vi = glXChooseVisual(dpy, DefaultScreen(dpy), visualAttr);
    if (vi == NULL) {
        fprintf(stderr, "No matching visual on display `%s'\n",
                getenv("DISPLAY"));
        exit(EXIT_FAILURE);
    }
    (void) glXGetConfig(dpy, vi, GLX_RGBA, &isRgba);
    /* create a GLX context */
    if ((*ctx = glXCreateContext(dpy, vi, 0, GL_TRUE)) == NULL){
       fprintf(stderr,"Cannot create a context.\n");
       exit(EXIT_FAILURE);
    }
    /* create a colormap (it's empty for rgba visuals) */
    cmap = XCreateColormap(dpy, RootWindow(dpy, vi->screen),
                           vi->visual, isRgba ? AllocNone : AllocAll);
    if (cmap == NULL) {
        fprintf(stderr,"Cannot create a colormap.\n");
        exit(EXIT_FAILURE);
    }
    /* fill the colormap with something simple */
    if (!isRgba) {
        static char *cnames[] = {
            "black","red","green","yellow","blue","magenta","cyan","white"
        };
                        
        for (i = 0; i < sizeof(cnames)/sizeof(cnames[0]); i++) {
            XStoreNamedColor(dpy, cmap, cnames[i], i, DoRed|DoGreen|DoBlue);
        }
    }
 
    /* create a window */
    swa.colormap = cmap;
    swa.border_pixel = 0;
    swa.event_mask = StructureNotifyMask |KeyPressMask | ExposureMask;

    *win = XCreateWindow(dpy, RootWindow(dpy, vi->screen),
                        init_x, init_y, xsize, ysize,
                        0, vi->depth, InputOutput, vi->visual,
                        CWBorderPixel | CWColormap | CWEventMask, &swa);
    if (*win == NULL) {   
        fprintf(stderr,"Cannot create a window.\n");
        exit(EXIT_FAILURE);
    }
    XStoreName(dpy, *win, name);  
    XSetNormalHints(dpy, *win, &hints);
    		/* If the user requested no window borders kill the borders */
    if (!win_border)		
	noborders(dpy, *win); 

    XMapWindow(dpy, *win);
    XIfEvent(dpy, &event, waitForMapNotify, (XPointer) win);
    /* Connect the context to the window */
    if (!glXMakeCurrent(dpy, *win, *ctx)) {
        fprintf(stderr, "Can't make window current to context\n");
        exit(EXIT_FAILURE);
    }
}


