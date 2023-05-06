/*
 * contcapt.c
 *
 * This program captures and displays continuous frames of video from
 * the IndigoVideo board.  Frames are captured using
 * svInitContinousCapture, svGetCaptureData, and svUnlockCaptureData.
 * If the window is resized by the user, the program computes the
 * new size, and then reinitalizes the capture mechanism.
 * Frames are always captured in SV_RGB8_FRAMES mode. 
 *
 * Usage:  contcapt [-d] [-y] [-b] [-f framerate] [-q queuesize]
 *   -d		print debug messages
 *   -y		capture YUV frames
 *   -b		capture YUV frames with blanking buffer
 *   -f num 	specifies the number of frames to capture per second
 *   -q	num	specifies the capture queue size
 * Hit Escape key to quit.
 */

#ident "$Revision: 1.15 $"

#include <stdio.h>
#include <stdlib.h>
#include <svideo.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <getopt.h>

#define OPT_STR "dyb?f:q:"

#define DEF_FRAME_RATE	15
#define MAX_RATE	30
#define DEF_BUF_SIZE	2
#define MIN_X_SIZE	120
#define MIN_Y_SIZE	90
#define MAX_X_SIZE	SV_PAL_XMAX
#define MAX_Y_SIZE	(SV_PAL_YMAX + SV_PAL_BLANKING_BUFFER_SIZE)

/* rgbbuf can't be an automatic variable */
static char rgbbuf[MAX_X_SIZE * MAX_Y_SIZE * sizeof(long)];

static void
usage(void)
{
    printf("Usage:\n");
    printf("\t-d	print debug messages\n");
    printf("\t-f rate	frame/sec rate (default: %d)\n", DEF_FRAME_RATE );
    printf("\t-q size	circular buffer size (default: %d)\n", DEF_BUF_SIZE);
    printf("\t-y 	capture YUV frames\n");
    printf("\t-b 	capture YUV frames with blanking buffer\n");
}

static void
get_max_size(SVhandle V, int f, long *w, long *h)
{
    long param[2];

    /* Determine max window size based on broadcast std and capture format */
    param[0] = SV_BROADCAST;
    if (svGetParam(V, param, 2) < 0) {
	svPerror("getparam");
	exit(1);
    }
    if (param[1] == SV_PAL) {
	*w = SV_PAL_XMAX;
	*h = SV_PAL_YMAX;
	if (f == SV_YUV411_FRAMES_AND_BLANKING_BUFFER)
	    *h += SV_PAL_BLANKING_BUFFER_SIZE;
    } else {
	*w = SV_NTSC_XMAX;
	*h = SV_NTSC_YMAX;
	if (f == SV_YUV411_FRAMES_AND_BLANKING_BUFFER)
	    *h += SV_NTSC_BLANKING_BUFFER_SIZE;
    }
}

main(int argc, char **argv)
{
    SVhandle V;
    long win;
    short val;
    int errflg, ch, hz;
    long width, height;
    void *captureData;
    svCaptureInfo svci;
    int framerate = DEF_FRAME_RATE;
    int bufsize = DEF_BUF_SIZE;
    int format = SV_RGB8_FRAMES;
    int debug = 0;
    int oldw, oldh, maxw, maxh;


    errflg = 1;
    while ((ch = getopt(argc, argv, OPT_STR)) != -1) {
        errflg = 0;
	switch (ch)	{   
	    case 'd': debug++;
	              break;
	    case 'f': framerate = atoi(optarg);
		      if (framerate < 1)
		          framerate = 1;
		      else if (framerate > MAX_RATE)
		          framerate = MAX_RATE;
	              break;
	    case 'y': format = SV_YUV411_FRAMES;
	              break;
	    case 'b': format = SV_YUV411_FRAMES_AND_BLANKING_BUFFER;
	              break;
	    case 'q': bufsize = atoi(optarg);
	              break;
	    default:  usage();
	              exit(1);
	}
    }
    if (errflg && (argc > 1)) {
        usage();
        exit(1);
    }

    /* Open the video device */
    if ((V = svOpenVideo()) == NULL) {
	svPerror("open");
	exit(1);
    }

    /* Determine maximum window size based on broadcast standard */
    get_max_size(V, format, &width, &height);

    /* Allow resizing window if capturing RGB frames, which can be scaled */
    if (format == SV_RGB8_FRAMES) {
	keepaspect(width, height);
	maxsize(width, height);
	stepunit(8, 6);
	minsize(MIN_X_SIZE, MIN_Y_SIZE);
    } else { 
	prefposition(200, 200+width-1, 100, 100+height-1);
    }

    /* Open the window */
    foreground();
    win = winopen("Continuous Capture");
    RGBmode();
    gconfig();
    if (format == SV_RGB8_FRAMES) {
    	getsize(&width, &height);
	pixmode(PM_SIZE, 8);	/* pixels are 8 bits */
    } else
	pixmode(PM_SIZE, 32);	/* pixels are 32 bits */

    if (debug)
        fprintf(stderr, "window size: %d X %d\n", width, height);

    svci.width = width;
    svci.height = height;
    svci.format = format;
    svci.size = bufsize;
    svci.samplingrate = 30 / framerate;

    if (svInitContinuousCapture(V, &svci)< 0) {
	svPerror("init cap");
	exit(1);
    }
    if (debug) {
        fprintf(stderr, 
		"using format %d, width %d, height %d, sample rate %d\n",
		svci.format, svci.width, svci.height, svci.samplingrate);
    }

    /* Event loop */
    hz = getgdesc(GD_TIMERHZ);
    noise(TIMER0, hz / framerate);
    qdevice(TIMER0);
    qdevice(WINQUIT);
    qdevice(WINSHUT);
    qdevice(ESCKEY);
    qdevice(VIDEO);

    while (1) {
	switch (qread(&val)) {
	case REDRAW:
	    oldw = width; oldh = height;
	    getsize(&width, &height);
	    if ((oldw != width) || (oldh != height)) {
	        if (svEndContinuousCapture(V) < 0) {
	            svPerror("redraw end cap");
		    exit(0);
	        }
	        viewport(0, width-1, 0, height-1);
	        if (debug)
	            fprintf(stderr, "redraw: %d %d\n", width, height);

	        svci.width = width;
	        svci.height = height;
	        if (svInitContinuousCapture(V, &svci) < 0) {
	            svPerror("redraw init cap");
		    exit(0);
	        }
		width = svci.width;
		height = svci.height;
	        if (debug) {
		    fprintf(stderr, "using width %d, height %d, rate %d\n",
			svci.width, svci.height, svci.samplingrate);
	        }
	    }
	    break;

	case VIDEO:
	    /*
	     * If broadcast standard changed, then resolution of video
	     * source has also changed. If capturing RGB frames,
	     * must restart continuous capture to set new decimation
	     * to keep window size the same even with new video resolution.
	     * If capturing YUV frames, which can't be scaled,
	     * must resize window to display new resolution.
	     */
	    if (val == SvEncodingAttribute) {
		if (svEndContinuousCapture(V) < 0) {
		    svPerror("video event end cap");
		    exit(0);
		}
		if (svci.format == SV_RGB8_FRAMES) {
		    get_max_size(V, format, &maxw, &maxh);
		    maxsize(maxw, maxh);
		    keepaspect(maxw, maxh);
		} else {
		    get_max_size(V, format, &svci.width, &svci.height);
		    prefsize(svci.width, svci.height);
		}
		winconstraints();
		if (svInitContinuousCapture(V, &svci) < 0) {
		    svPerror("video event init cap");
		    exit(0);
		}
	    }
	    break;

	case TIMER0: {
		long fieldID;
		svGetCaptureData(V, &captureData, &fieldID);
		if (debug > 1)
		    fprintf(stderr, "timer %d\n", fieldID);
		if (captureData) {
		    if (svci.format == SV_RGB8_FRAMES)
			svInterleaveFields(TRUE, captureData, rgbbuf,
				svci.width, svci.height);
		    else
			svYUVtoRGB(TRUE, captureData,
				(long *)rgbbuf, svci.width, svci.height);
		    svUnlockCaptureData(V, captureData);
		    lrectwrite(0, 0, svci.width-1, svci.height-1,
			    (unsigned long *) rgbbuf);
		} 
	    }
	    break;

	case ESCKEY:
	    if (val)    /* exit on key up */
		break;
	case WINQUIT:
	case WINSHUT:
	    svEndContinuousCapture(V);
	    svCloseVideo(V);
	    winclose(win);
	    exit(0);
	    break;
	}
    }
}
