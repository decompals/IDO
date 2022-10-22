/*
 * File: 	contcapt.c
 *
 * Usage:	contcapt [-b numofbuffers] [-c numofframes] [-d] [-D]
 *                       [-f] [-F] [-g wxh] [-i frames] [-m] [-n devicenum]
 *			 [-r rate] [-t]	[-v videosource] [-w] [-z n/d] [-h]
 *
 * Description:	Contcapt demonstrates continuous capture using buffering 
 *				and sproc.
 *
 * Functions:	SGI Video Library functions used (see other modules)
 *
 *		vlOpenVideo()
 *		vlGetDeviceList()
 *		vlGetNode()
 *		vlCreatePath()
 *		vlGetDevice()
 *		vlSetupPaths()
 *		vlRegisterHandler()
 *		vlSelectEvents()
 *		vlSetControl()
 *		vlGetControl()
 *		vlRegisterBuffer()
 *		vlAddCallback()
 *		vlBeginTransfer()
 *		vlMainLoop()
 *		vlEndTransfer()
 *		vlDeregisterBuffer()
 *		vlDestroyPath()
 *		vlCloseVideo()
 *		vlPerror()
 *		vlStrError()
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>

#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <getopt.h>
#include <sys/time.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <GL/gl.h>
#include <GL/GLwMDrawA.h>
#include <gl/image.h>

#include <dmedia/vl.h>

#define VL_PACKING_INVALID	-1
#define Debug			if (debug) printf
#define TIME_ARRAY_SIZE		400	/* MUST be divisible by 5! */
#define TIME_GRANULARITY	1000


/*
 *  Global variables
 */

char *cmdargs = "b:c:CdDfFg:hi:mn:or:tv:wz:";

char *usage_m =
	"usage: %s %s\n"
	"where arguments may be:\n"
	"	-b n	use n buffers\n"
	"	-c n	capture n frames\n"
	"	-C	use software color space converter\n"
	"	-d	turn off double buffer mode\n"
	"	-D	turn on debug mode (displays more information)\n"
	"	-f	fast mode (no display)\n"
	"	-F	specifies field (or non-interleaved) mode\n"
	"	-g wxh	frame geometry (ex. 640x480)\n"
	"	-i n	report statistics every n frames\n"
	"	-m	monochrome input\n"
	"	-n	device number\n"
        "       -o      Open GL Mode\n"
	"	-r n	set frame rate to n\n"
	"	-t	turn on timing statistics gathering\n"
	"	-v	video source (depends on hardware; see release notes)\n"
	"	-w	32 bit 'wide' RGB pixels\n"
	"	-z d/d	set zoom factor to fraction 'd/d'\n"
	"	-h	this help message\n"
;

static char 	*dataPtr = NULL;
char *		_progName;
int		buffers = 2;
int		packing = VL_PACKING_INVALID;
int		totalCount = 10000000;
int		debug = 0;
int		devicenum = -1;
VLDev		deviceId;
int		double_buffer = 1;
int		fast = 0;
int		fieldmode = 0;
int		frameCount;
int		frameInterval = -1;
int		framenumber = 0;
int		lasttime = 0;
int		rate = 0;
int		secs;
int		time_array[TIME_ARRAY_SIZE];
int		time_on = 0;
int		xsize = 0;
int		ysize = 0;
int		zoom_num = 1, zoom_denom = 1;
int		vidsrc = VL_ANY;
struct timeval	tv_save;
VLPath		vlPath;
VLServer	vlSvr;
VLNode		drn;
VLNode		src;
VLBuffer	transferBuf;
static Display *dpy;
static XtAppContext app_context;

/* The following is used to handle single and double
   buffer OpenGL widgets. Note that in IRIS GL, one can 
   change the buffering 'on the fly' via a call to 
   singlebuffer() or doublebuffer(). Since we cannot
   do this in OpenGL, we have two buffers, one single
   and one double. We only manage the one we wish to 
   use (an unmanage the other one). */

/* Encapsulate gl widget with its context */
typedef struct glContainer {	
    Widget widget;
    GLXContext context;
} glContainer;

static glContainer single_gl, double_gl, *current_gl;

/*
 * Function protoypes 
 */
 
void		processVLEvent(uint fd, void *source);
void	 	processXEvent(VLServer, VLEvent *, void *);
void		exitcapture(void);
void		initStatistics(void);
void		reportStatistics(void);
void		getcmdargs(int argc, char **argv);
char *		packing_name (int packtype);
Widget		openWindow(int, char **, int x, int y, int doublebuffer);


/* Get the start time (for frame rate measurement) */
void
initStatistics(void)
{
    if (!debug) return;
    
    gettimeofday(&tv_save);
}

/* Calculate and display the frame rate */
void
reportStatistics(void)
{
    double rate;
    struct timeval tv;
    int delta_t;
    
    gettimeofday(&tv);
    
    /* Delta_t in microseconds */
    delta_t = (tv.tv_sec*1000 + tv.tv_usec/1000) -
	      (tv_save.tv_sec*1000 + tv_save.tv_usec/1000);
    
    rate = frameCount*1000.0/delta_t;
    
    printf("got %5.2f %s/sec\n", rate,  fieldmode? "fields" : "frames");
    tv_save.tv_sec = tv.tv_sec;
    tv_save.tv_usec = tv.tv_usec;
}

char *
packing_name (int packtype)
{
    switch (packtype) 
    {
	case VL_PACKING_RGB_8:    
	    return "RGB";
	break;
	
        case VL_PACKING_RGBA_8:   
	    return "RGBA";
	break;
	
	case VL_PACKING_RBG_323:   
	    return "Starter Video RGB8";
	break;
	
	case VL_PACKING_RGB_332_P:	
	    return "RGB332";
	break;
	
	case VL_PACKING_Y_8_P: 
	    return "8 Bit Greyscale";
	break;

        case VL_PACKING_ABGR_8:
            return "RGBA_8 (OpenGL) ";
	
	default:		
	    return "???";
	break;
    }
}

/* Parse the command line args */
void
getcmdargs(int argc, char **argv)
{
    int c;
    int len, xlen, ylen;
    char *numbuf;
    char *xloc;
   
    while ((c = getopt(argc, argv, cmdargs)) != EOF) 
    {
	switch (c) 
	{

	    case 'b':
		buffers = atoi(optarg);
	    break;
    
	    case 'c':
		totalCount = atoi(optarg);
	    break;
    
	    case 'D':
		debug = 1;
	    break;
    
	    case 'd':
		double_buffer = 0;
	    break;
    
	    case 'f':
		fast = 1;
		packing=VL_PACKING_YVYU_422_8;
	    break;
    
	    case 'F':
		fieldmode = 1;
	    break;
    
	    case 'g':
		len = strlen(optarg);
		xloc = strchr(optarg, 'x');
		if (!xloc) 
		{
		    printf("Error: invalid geometry format, using default size\n");
		    break;
		}
		xlen = len - strlen(xloc);
		if (xlen < 1 || xlen > 4) 
		{
		    printf("Error: invalid x size, using default size\n");
		    break;
		}
                numbuf = strtok(optarg,"x");
		xsize = atoi(numbuf);
    
		ylen = len - xlen -1;
		if (ylen < 1 || ylen > 4) 
		{
		    printf("Error: invalid y size, using default size\n");
		    break;
		}
                numbuf = strtok(NULL,"");
		ysize = atoi(numbuf);
		break;
    
	    case 'i':
		frameInterval = atoi(optarg);
		printf("frameInterval = %d\n", frameInterval);
	    break;
		
	    case 'm':
		packing = VL_PACKING_Y_8_P;
	    break;
    
	    case 'o':
		packing = VL_PACKING_ABGR_8;
	    break;
    
	    case 'n':
		devicenum = atoi(optarg);
		printf("using device number: %d\n", devicenum);
	    break;
    
	    case 'r':
		rate = atoi(optarg);
	    break;
    
	    case 't':
		time_on = 1;
	    break;
    
	    case 'v':
		vidsrc = atoi(optarg);
	    break;
    
	    case 'w':
		packing = VL_PACKING_RGB_8;
	    break;
		
	    case 'z':
		if (sscanf(optarg,"%d/%d", &zoom_num, &zoom_denom) != 2 ||
				!zoom_num || !zoom_denom)
		{
		    fprintf(stderr, "%s: ERROR: zoom format <num>/<denom>\n",
				    _progName);
		    exit(1);
		}
	    break;
		
	    case 'h':
	    default:
		fprintf(stderr, usage_m, _progName, cmdargs);
		exit(0);
	    break;
	}
    }
}

main(int argc, char **argv)
{
    VLDevList devlist;
    char *deviceName;
    char window_name[128];
    char *xloc;
    long win;
    int c;
    int len, xlen, ylen;
    VLControlValue val;
    Widget mainWindow;

    _progName = argv[0];

    /* Parse command line args */
    getcmdargs(argc, argv);

    /* Clear the array that will hold timestamps of captured frames */
    if (time_on)
	for (c = 0; c < TIME_ARRAY_SIZE; c++)
	    time_array[c] = 0;

    /* Connect to the video daemon */
    if (!(vlSvr = vlOpenVideo(""))) 
    {
	printf("couldn't open video\n");
	exit(1);
    }

    /* Get the list of the devices that the daemon supports */
    if (vlGetDeviceList(vlSvr, &devlist) < 0) 
    {
	printf("error getting device list: %s\n",vlStrError(vlErrno));
	exit(1);
    }

    /* Set up a source node on the specified video source */
    Debug("using video source number: %d\n", vidsrc);
    src = vlGetNode(vlSvr, VL_SRC, VL_VIDEO, vidsrc);

    /* Set up a drain node in memory */
    drn = vlGetNode(vlSvr, VL_DRN, VL_MEM, VL_ANY);
  
    /* 
     * Create a path from video to memory. 
     * If the user did not specify a device on the command        
     * line , i.e. devicenum = -1, use the first device 
     * that supports such a path. 
     */
    if (devicenum == -1) /* No user-specified device */
    { 
	int i;

	vlPath = vlCreatePath(vlSvr, VL_ANY, src, drn);
	if (vlPath < 0) 
	{
	    printf("can't create path: %s\n", vlStrError(vlErrno));
	    exit(1);
	}
	/* Get the device number for future use */
	deviceId = vlGetDevice(vlSvr, vlPath); 
	for (i=0; i<devlist.numDevices; i++)
	    if (devlist.devices[i].dev == deviceId) {
		devicenum = i;
		break;
	    }
    } 
    else /* User specified a device */
    {
	deviceId = devlist.devices[devicenum].dev;
	if ((vlPath = vlCreatePath(vlSvr, deviceId, src, drn)) < 0) {
	    vlPerror("vlCreatePath");
	    exit(1);
	}
    }
    
    /* Set up the hardware for and define the usage of the path */
    if (vlSetupPaths(vlSvr, (VLPathList)&vlPath, 1, VL_SHARE, VL_SHARE) < 0)
    {
	printf ("could not setup a vid to mem path\n");
	exit(1);
    }

    /* Get the name of the device we're using */
    deviceName = devlist.devices[devicenum].name;

    /* Set the zoom ratio */
    val.fractVal.numerator = zoom_num;
    val.fractVal.denominator = zoom_denom;
    if (vlSetControl(vlSvr, vlPath, drn, VL_ZOOM, &val) < 0) {
	if (vlGetControl(vlSvr, vlPath, drn, VL_ZOOM, &val) < 0) {
	    printf("Fatal error: Unable to set zoom\n");
	}
	printf("Requested zoom could not be set on device. Using %d/%d.\n",
	    val.fractVal.numerator, val.fractVal.denominator);
    }
    
    /* Set the frame rate */
    if (rate) 
    {
	val.fractVal.numerator = rate;
	val.fractVal.denominator = 1;
	vlSetControl(vlSvr, vlPath, drn, VL_RATE, &val);
    }

    vlGetControl(vlSvr, vlPath, drn, VL_RATE, &val);
    Debug("frame rate = %d/%d\n", val.fractVal.numerator,
	  val.fractVal.denominator);

    /* 
     * Specify what vlPath-related events we want to receive.
     * In this example we only want transfer complete, transfer
     * failed and stream preempted events.
     */
    vlSelectEvents(vlSvr, vlPath, VLTransferCompleteMask|
                              VLStreamPreemptedMask|VLTransferFailedMask);

    if (vlGetControl(vlSvr, vlPath, drn, VL_CAP_TYPE, &val) < 0)
	vlPerror("Getting VL_CAP_TYPE");

    Debug("cap type = %d\n", val.intVal);

    /* Set up for non-interleaved frame capture */
    if (fieldmode) 
    {
	val.intVal = VL_CAPTURE_NONINTERLEAVED;
	if (vlSetControl(vlSvr, vlPath, drn, VL_CAP_TYPE, &val) < 0)
	    vlPerror("Setting VL_CAP_TYPE");
	    
	/* Make sure non-interleaved capture is supported by the hardware */
	if (vlGetControl(vlSvr, vlPath, drn, VL_CAP_TYPE, &val) < 0)
	    vlPerror("Geting VL_CAP_TYPE");
	if (val.intVal != VL_CAPTURE_NONINTERLEAVED)
	    fprintf(stderr,"Capture type not set to NONINTERLEAVED: %d\n",
		val.intVal);
	Debug("Field (non-interleaved) mode\n");
	vlGetControl(vlSvr, vlPath, drn, VL_RATE, &val);
	Debug("field rate = %d/%d\n", val.fractVal.numerator,
	      val.fractVal.denominator);
    }

    /* Set the video geometry */
    if (xsize) 
    {
	val.xyVal.x = xsize;
	val.xyVal.y = ysize;
	vlSetControl(vlSvr, vlPath, drn, VL_SIZE, &val);
    }

    /* Make sure this size is supported by the hardware */
    vlGetControl(vlSvr, vlPath, drn, VL_SIZE, &val);
    xsize = val.xyVal.x;
    ysize = val.xyVal.y;
    Debug("grabbing size %dx%d\n", val.xyVal.x, val.xyVal.y);

    /* Set the window to the same geometry as the video data */

    sprintf (window_name, "Continuous Capture %s", deviceName);
    /* Create a graphics window that will display the video data */
    /* Set up to double buffer the data */
    /* Set the graphics to RGB mode */

    mainWindow = openWindow(argc, argv, xsize, ysize, double_buffer);
    dpy = XtDisplay(mainWindow);

    if (double_buffer) 
	Debug("double buffer mode\n");

    /* Set the packing type for the video board */
    if (packing != VL_PACKING_INVALID)
    {
	val.intVal = packing;
	if (vlSetControl(vlSvr, vlPath, drn, VL_PACKING, &val) < 0)
	    vlPerror("Setting VL_PACKING");
    }
    
    /* Set the graphics to the same packing as the video board */
    vlGetControl(vlSvr, vlPath, drn, VL_PACKING, &val);
    packing = val.intVal;
    switch (val.intVal)
    {
	case VL_PACKING_Y_8_P:
	/*
	 * XXX -gordo
	 *  this needs to restore the colormap
	 */

#ifdef NOTDEF /* Can't change these modes 'on the fly' */

	    {
		int i;
    		
		/* Use colormap mode for greyscale */
		cmode();			
		for (i = 0; i < 256; i++)
		    mapcolor(i, i, i, i);
		pixmode(PM_SIZE, 8);	/* Pixels are 8 bit Greyscale */
		gconfig();      /* Reconfigure graphics */
	    }
	break;
	
	case VL_PACKING_RGB_332_P:
	    /*pixmode(PM_SIZE, 9);	/* Pixels are 8 bit RGB */
	break;
	
	case VL_PACKING_RBG_323:
	    /*pixmode(PM_SIZE, 8); */
	break;
#endif
	break;

	/*
	case VL_PACKING_YVYU_422_8:
	break;
	*/

	/* Unknown packing type, use RGB as the default */
	default:		
	    packing = val.intVal = VL_PACKING_RGB_8;
	    if (vlSetControl(vlSvr, vlPath, drn, VL_PACKING, &val) < 0)
		vlPerror("Setting RGB");
	    vlGetControl(vlSvr, vlPath, drn, VL_PACKING, &val);
	    Debug("packing type now is %s\n", packing_name(val.intVal));
	    if (val.intVal != packing)
	    {
		fprintf(stderr, "%s: Error, could not set Packing to %s", _progName,
			packing_name (packing));
		exit(1);
	    }
	    /*
	     * There is no break here intentionally... the packing should
	     * now be VL_PACKING_RGB, and thus should fall through...
	     */
	case VL_PACKING_RGB_8:
	case VL_PACKING_ABGR_8:
	case VL_PACKING_RGBA_8:
	break;
    }

    vlGetControl(vlSvr, vlPath, drn, VL_SIZE, &val);
    if (xsize != val.xyVal.x || ysize != val.xyVal.y) {
	xsize = val.xyVal.x;
	ysize = val.xyVal.y;
#ifdef NOTDEF
	prefsize(xsize, ysize);
	winconstraints();
	reshapeviewport(); 
#endif
	Debug("after VL_PACKING size is now %dx%d\n", xsize, ysize);
    }

    /* Set up the ring buffer for data transfer */
    transferBuf = vlCreateBuffer(vlSvr, vlPath, drn, buffers);
    
    /* Associate the ring buffer with the path */
    vlRegisterBuffer(vlSvr, vlPath, drn, transferBuf);
    
    /*
     * Set processVLEvent() as the callback for a transfer complete,
     * transfer failed and stream preempted events */
    vlAddCallback(vlSvr, vlPath, VLTransferCompleteMask|VLStreamPreempted
                      |VLTransferFailedMask, (VLCallbackProc) processVLEvent, vlSvr);
    
    /* Specify a file descriptor and pending check function for VL events */
    vlRegisterHandler(vlSvr, vlConnectionNumber(vlSvr), (VLEventHandler)processVLEvent,
                (VLPendingFunc)vlPending, vlSvr);

    /* Specify a file descriptor and pending check function for X events */
    vlRegisterHandler(vlSvr, ConnectionNumber(dpy), (VLEventHandler)processXEvent,
                (VLPendingFunc)XPending, dpy);

    /* Begin the data transfer */
    if (vlBeginTransfer(vlSvr, vlPath, 0, NULL) < 0)
    {
	vlPerror("vlBeginTransfer");
	exit(1);
    }
    
    /* Get the start time used in calculating frame rate */
    initStatistics();

    /* Main event loop */
    vlMainLoop();
}

/* Handle video library events (only transfer complete events in this example) */
void
processVLEvent(uint fd, void *source)
{
    int count;
    static int frameNum;
    static ulong lasttime = -1;
    static ulong lastsec;
    int timeDiff;
    DMediaInfo *dmInfo;
    VLInfoPtr info;
    VLEvent ev;

    /* Display the frame rate at a user specified interval */
    if (frameInterval > 0 && frameCount > frameInterval)
    {
        reportStatistics();
        frameCount = 0;
    }

    vlNextEvent(vlSvr, &ev);

    switch (ev.reason)
    {
        case VLTransferComplete:
            framenumber++;
                
            /* Get a pointer to the most recently captured frame */
            info = vlGetLatestValid(vlSvr, transferBuf);
            if (!info)
                break;

            /* Get the valid video data from that frame */
            dataPtr = vlGetActiveRegion(vlSvr, transferBuf, info);
        
            /* Time stamp the frame */
            if (time_on)
            {
                dmInfo = vlGetDMediaInfo(vlSvr, transferBuf, info);
                if (lasttime != -1)
                    {
                        timeDiff = (dmInfo->time.tv_sec - lastsec) * TIME_GRANULARITY;
                        timeDiff += (int)(dmInfo->time.tv_usec - lasttime + 500) /
                            (1000000 / TIME_GRANULARITY);
                        if (timeDiff >= TIME_ARRAY_SIZE)
                            timeDiff = TIME_ARRAY_SIZE-1;
                        time_array[timeDiff] += 1;
                    }
                lasttime = dmInfo->time.tv_usec;
                lastsec = dmInfo->time.tv_sec;
            }
        
            /* If the user didn't request fast capture write the frame to the screen */
            if (current_gl->context) 
			{
		   		GLwDrawingAreaMakeCurrent(current_gl->widget, current_gl->context);


				glRasterPos3i(-1,1,0);

   				/* 
   			 	 * Set the GL to lrectwrite from top to bottom
   			 	 * (the default is top to bottom). This way the
   			 	 * sense of the video frame is the same as that 
   			 	 * of the graphics window.
  			 	 */

   				glPixelZoom(1, -1);
				switch(packing) {
					case VL_PACKING_ABGR_8:
              			glDrawPixels(xsize, ysize, GL_RGBA, GL_UNSIGNED_BYTE, (ulong *)dataPtr);
					break;
		
					case VL_PACKING_RGB_8:
           				glDrawPixels(xsize, ysize, GL_ABGR_EXT, GL_UNSIGNED_BYTE, (ulong *)dataPtr);
					break;

					case VL_PACKING_Y_8_P:
						glDrawPixels(xsize, ysize, GL_LUMINANCE, GL_UNSIGNED_BYTE, (ulong *)dataPtr);
					break;
				}
				if (double_buffer)
                  		glXSwapBuffers(XtDisplay(current_gl->widget), XtWindow(current_gl->widget));
            }
       	 
           	/* Done with that frame, free the memory used by it */
           	vlPutFree(vlSvr, transferBuf);
           	frameCount++;

           	/* If we've gotten all the frames requested, quit */
           	if (framenumber >= totalCount)
            {
            	if (frameInterval > 0)
               		reportStatistics();
                exitcapture();
			} 
       	break;

        case VLTransferFailed:    
            exitcapture();
        break;

        case VLStreamPreempted: 
            fprintf(stderr, "%s: Stream was preempted by another Program\n",
                _progName);
            exitcapture();
        break;

        default:
            /*printf("Got Event %d\n", ev->reason);*/
	break;
    }
}


void
processXEvent(VLServer svr, VLEvent *event, void *data)
{
    XEvent ev;
    XtAppNextEvent(app_context, &ev);

    XtDispatchEvent(&ev);
   
#ifdef NOTDEF
	/* Check for window quit events */
    if (ev.type = ClientMessage) 
		if (ev.xclient.message_type == WM_PROTOCOLS)
			if (ev.xclient.data.l[0] == WMDELETE_WINDOW)
				exitcapture();
#endif

}

void
exitcapture()
{
	int loop, j;

	    
	/* End the data transfer */
	vlEndTransfer(vlSvr, vlPath);

	/* Disassociate the ring buffer from the path */
	vlDeregisterBuffer(vlSvr, vlPath, drn, transferBuf);

	/* Destroy the path, free the memory it used */
	vlDestroyPath(vlSvr,vlPath);
	
	/* Destroy the ring buffer, free the memory it used */
	vlDestroyBuffer(vlSvr, transferBuf);

	/* Disconnect from the daemon */
	vlCloseVideo(vlSvr);

	/* Print the time stamps from the captured frames */
	if (time_on)
	    for (loop = 0; loop < TIME_ARRAY_SIZE; ) {
		printf("%3d: ", loop);
		for (j = 0; j < 5; j++)
		    printf("\t%d", time_array[loop++]);
		printf("\n");
	    }

	exit(0);
}

static void 
initCB(Widget w, XtPointer client, XtPointer call_data)
{
    glContainer *glc = (glContainer *) client;
    Arg args[10]; int n;
    XVisualInfo *vi;

    n = 0;
    XtSetArg(args[n], GLwNvisualInfo, &vi); n++;
    XtGetValues(w, args, n);

    if (!(glc->context))
    	glc->context = glXCreateContext(XtDisplay(w), vi, 0, GL_TRUE);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(-0.5, -0.5, xsize, ysize,0,0);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glClearColor(0,0,1, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);
}

static void 
exposeCB(Widget w, XtPointer client, GLwDrawingAreaCallbackStruct *call_data)
{
    GLfloat val;
    glContainer *glc = (glContainer *) client;
    GLint viewx = (GLint) call_data->width;
    GLint viewy = (GLint) call_data->height;

    GLwDrawingAreaMakeCurrent(w, glc->context);
    current_gl = glc;
}

static void 
resizeCB(Widget w, XtPointer client_data, GLwDrawingAreaCallbackStruct *call_data)
{
    GLint viewx = (GLint) call_data->width;
    GLint viewy = (GLint) call_data->height;

    printf("resizeCB...\n");

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glViewport(0, 0, viewx-1, viewy-1);
    glOrtho(-0.5, -0.5, viewx, viewy, 0, 1);
}

static void
inputCB(Widget w, XtPointer client_data, XtPointer call_data)
{
    KeySym keysym;
    GLwDrawingAreaCallbackStruct *glw = (GLwDrawingAreaCallbackStruct *) call_data;
    char buffer[10];

    if (glw->event->type == KeyRelease) {
	if (XLookupString((XKeyEvent*)glw->event,buffer,sizeof(buffer),&keysym,NULL) == 1) {
		switch(keysym) {
			case XK_Escape: exitcapture();	
					break;
			case XK_d:	XtUnmanageChild(current_gl->widget);
					Debug("Setting %s buffer mode\n", (current_gl == &double_gl)?
							"double":"single");
					current_gl = (current_gl == &single_gl) ? &double_gl : &single_gl;
					XtManageChild(current_gl->widget);
					double_buffer = (current_gl == &single_gl) ? 0:1;
					break;
			case XK_q:	exitcapture();
					break;
		}
	}
    }
}

Widget openWindow(int argc, char **argv, int sizeX, int sizeY, int doubleBuffer)
{
    Widget toplevel, form, glw;
    Arg args[20];
    int n;
	int noargs = 1;	/* make sure we ignore arguments */

    toplevel = XtAppInitialize(&app_context, "4DgiftsGLw",
                               (XrmOptionDescList) NULL,
                               (Cardinal)0,
                               (int *)&noargs,
                               (String*)argv,
                               0, /*fallback_resources,*/
                               (ArgList)NULL, 0);

    n = 0;
    form = XmCreateForm(toplevel, "form", args, n);
    XtManageChild(form);

    /* Create single buffer widget */
    n = 0; 
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNwidth, sizeX); n++;
    XtSetArg(args[n], XmNheight, sizeY); n++;
    XtSetArg(args[n], GLwNdoublebuffer, FALSE); n++;
    XtSetArg(args[n], GLwNrgba, TRUE); n++;
    glw = single_gl.widget = GLwCreateMDrawingArea(form, "glwidgetSingle", args, n);
    single_gl.context = NULL;

    XtAddCallback(glw, GLwNexposeCallback, (XtCallbackProc) exposeCB, &single_gl);
    XtAddCallback(glw, GLwNresizeCallback, (XtCallbackProc) resizeCB, &single_gl);
    XtAddCallback(glw, GLwNginitCallback,  (XtCallbackProc) initCB, &single_gl);
    XtAddCallback(glw, GLwNinputCallback,  (XtCallbackProc) inputCB, &single_gl);

    /* Create double buffer widget */
    n = 0; 
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNwidth, sizeX); n++;
    XtSetArg(args[n], XmNheight, sizeY); n++;
    XtSetArg(args[n], GLwNdoublebuffer, FALSE); n++;
    XtSetArg(args[n], GLwNrgba, TRUE); n++;
    glw = double_gl.widget = GLwCreateMDrawingArea(form, "glwidgetDouble", args, n);
    double_gl.context = NULL;

    XtAddCallback(glw, GLwNexposeCallback, (XtCallbackProc) exposeCB, &double_gl);
    XtAddCallback(glw, GLwNresizeCallback, (XtCallbackProc) resizeCB, &double_gl);
    XtAddCallback(glw, GLwNginitCallback,  (XtCallbackProc) initCB, &double_gl);
    XtAddCallback(glw, GLwNinputCallback,  (XtCallbackProc) inputCB, &double_gl);

    if (doubleBuffer)  
	current_gl = &double_gl;
    else
        current_gl = &single_gl;

    XtManageChild(current_gl->widget);

    XtRealizeWidget(toplevel);

    return (glw);    /* this is the only widget we need to know about */
}

/* === */

