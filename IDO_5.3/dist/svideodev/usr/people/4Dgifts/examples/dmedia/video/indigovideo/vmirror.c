/*
 * vmirror.c
 *
 * This program captures continuous frames of video from the IndigoVideo board.
 * It performs some simple processing of the frames before displaying them.
 * The frames can be shown in reverse, with left or right mirroring, or
 * with four-fold symmetry. The type of mirroring is selected via a menu.
 * With the -c command-line option, frames are displayed using the GL colormap.
 *
 * Frames are captured using svInitContinousCapture, svGetCaptureData, and 
 * svUnlockCaptureData.  If the window is resized by the user, the program
 * computes the new size, and then reinitalizes the capture mechanism.
 * Frames are always captured in SV_RGB8_FRAMES mode. 
 *
 * Usage:  vmirror [-cd] [-f framerate] [-q queuesize]
 *   -c		use GL's color map mode instead of RGB mode
 *   -d		print debug messages
 *   -f num 	specifies the number of frames to capture per second
 *   -q	num	specifies the capture queue size
 * Hit the Escape key to quit.
 */

#ident "$Revision: 1.4 $"

#include <stdio.h>
#include <stdlib.h>
#include <svideo.h>
#include <string.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <getopt.h>


#define DEF_FRAME_RATE	15
#define MAX_RATE	30
#define DEF_BUF_SIZE	2
#define MIN_X_SIZE	120
#define MIN_Y_SIZE	90
#define MAX_X_SIZE	SV_PAL_XMAX
#define MAX_Y_SIZE	(SV_PAL_YMAX + SV_PAL_BLANKING_BUFFER_SIZE)

/* Constants for the menu items */
#define NONE	1
#define REVERSE	2
#define RHALF	3
#define LHALF	4
#define QUARTER	5
#define MAX_SYM 5
#define DEF_SYMMETRY RHALF
static char menustr[] = "Mirror %t|normal|reverse|right 2-fold|left 2-fold|4-fold %l|video map|normal map";

static void revcopy(char *, char *, int);
static void mirrorcopy(char *, char *, int, int, int, int);
static void makevideomap(void);


#define OPT_STR "cdf:q:"

static void
usage(void)
{
    printf("Usage:\n");
    printf("\t-c	use color map mode (default is RGB mode)\n");
    printf("\t-d	print debug messages\n");
    printf("\t-f rate	frame/sec rate (default: %d)\n", DEF_FRAME_RATE );
    printf("\t-q size	circular buffer size (default: %d)\n", DEF_BUF_SIZE);
}

main(int argc, char **argv)
{
    SVhandle V;
    long win;
    short val;
    int errflg, ch;
    long width, height;
    char *captureData;
    char rgbbuf[MAX_X_SIZE * MAX_Y_SIZE];
    long param[2];
    svCaptureInfo svci;
    int oldw, oldh;
    int	i, j;
    int framerate = DEF_FRAME_RATE;
    int bufsize = DEF_BUF_SIZE;
    int debug = 0, cmapmode = 0;
    int menu, symmetry = DEF_SYMMETRY;

    errflg = 1;
    while ((ch = getopt(argc, argv, OPT_STR)) != -1) {
        errflg = 0;
	switch (ch)	{   
	    case 'c': cmapmode++;
	              break;
	    case 'd': debug++;
	              break;
	    case 'f': framerate = atoi(optarg);
		      if (framerate < 1)
		          framerate = 1;
		      else if (framerate > MAX_RATE)
		          framerate = MAX_RATE;
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
    param[0] = SV_BROADCAST;
    svGetParam(V, param, 2);
    if (param[1] == SV_PAL) {
	width = SV_PAL_XMAX;
	height = SV_PAL_YMAX;
    } else {
	width = SV_NTSC_XMAX;
	height = SV_NTSC_YMAX;
    }

    /* Allow resizing window if capturing RGB frames, which can be scaled */
    keepaspect(width, height);
    maxsize(width, height);
    stepunit(8*2, 6*2);		/* multiple of 4 for 4-fold symmetry */
    minsize(MIN_X_SIZE, MIN_Y_SIZE);

    /* Open the window */
    win = winopen("Video Mirror");
    pixmode(PM_SIZE, 8);	/* pixels are 8 bits */
    if (cmapmode) {
	makevideomap();
    } else {
	char *s;

	RGBmode();
	gconfig();

	/* Remove the color map menu items */
	s = strstr(menustr, "%l");
	if (s)
	    *s = '\0';
    }
    menu = defpup(menustr);
    for (i = 1; i <= MAX_SYM; i++)
	if (i != DEF_SYMMETRY)
	    setpup(menu, i, PUP_BOX);
    setpup(menu, DEF_SYMMETRY, PUP_CHECK);

    getsize(&width, &height);
    if (debug)
        fprintf(stderr, "window size: %d X %d\n", width, height);

    svci.width = width;
    svci.height = height;
    svci.format = SV_RGB8_FRAMES;
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
    qdevice(WINQUIT);
    qdevice(WINSHUT);
    qdevice(ESCKEY);
    qdevice(VIDEO);
    qdevice(MENUBUTTON);

    while (1) {
	if (qtest()) {
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

	    case MENUBUTTON:
		if (val) {
		    int item = dopup(menu);
		    if (item > MAX_SYM) {
			switch (item - MAX_SYM) {
			    case 1: makevideomap(); break;
			    case 2: system("makemap"); break;
			}
		    } else if (item > 0 && item != symmetry) {
			setpup(menu, symmetry, PUP_BOX);
			setpup(menu, item, PUP_CHECK);
			symmetry = item;
		    }
		}
		break;

	    case VIDEO:
		/*
		 * If broadcast standard changed, then resolution of video
		 * source has also changed. If capturing RGB frames,
		 * must restart continuous capture to set new decimation
		 * to keep window size the same even with new video
		 * resolution.
		 */
		if (val == SvEncodingAttribute) {
		    if (svEndContinuousCapture(V) < 0) {
			svPerror("video event end cap");
			exit(0);
		    }
		    if (svInitContinuousCapture(V, &svci) < 0) {
			svPerror("video event init cap");
			exit(0);
		    }
		}
		break;

	    case ESCKEY:
		if (val)	/* exit on key up */
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

	svGetCaptureData(V, (void **)&captureData, NULL);
	if (captureData) {
	    int half = height / 2;

	    switch (symmetry) {
	    case NONE:
	    default:
		svInterleaveFields(TRUE, captureData, rgbbuf, width, height);
		break;

	    case REVERSE:
		for (i = 0, j = height - 2; i < half; i++, j -= 2) {
		    revcopy(&captureData[i*width],
		    	    &rgbbuf[j*width+width-1], width);
		}
		for (i = half, j = height - 1; i < height; i++, j -= 2) {
		    revcopy(&captureData[i*width],
		    	    &rgbbuf[j*width+width-1], width);
		}
		break;

	    case QUARTER: 
		/* Just use upper-left corner and replicate it */
		mirrorcopy(captureData, rgbbuf, height - 2,
			height/4, width, 1);
		mirrorcopy(&captureData[half*width], rgbbuf, height - 1,
			height/4, width, 1);
		for (i = 0, j = height - 1; i < half; i++, j--) {
		    bcopy(&rgbbuf[j*width], &rgbbuf[i*width], width);
		}
		break;

	    case RHALF:
	    case LHALF:
		mirrorcopy(captureData, rgbbuf, height - 2, 
			half, width, symmetry == LHALF);
		mirrorcopy(&captureData[half*width], rgbbuf, height - 1,
			half, width, symmetry == LHALF);
		break;
	    }
	    svUnlockCaptureData(V, captureData);
	    lrectwrite(0, 0, width-1, height-1, (unsigned long *) rgbbuf);
	} 
    }
}

/* Reverse byte copy:  src = left-to-right, dst = right-to-left */
static void
revcopy(char *src, char *dst, int len)
{
    while (len >= 8) {
	*dst-- = *src++;
	*dst-- = *src++;
	*dst-- = *src++;
	*dst-- = *src++;
	*dst-- = *src++;
	*dst-- = *src++;
	*dst-- = *src++;
	*dst-- = *src++;
	len -= 8;
    }
    while (len > 0) {
	*dst-- = *src++;
	len--;
    }
}

/* 
 * Routine that's used for interleaving fields into frames.
 * Copies half the line in normal direction then mirrors it.
 */
static void
mirrorcopy(char *src, char *dst, int dstart, int cnt, int width, int leftside)
{
    int whalf = width / 2;
    int i, j;

    if (leftside) {
	for (i = 0, j = dstart; i < cnt; i++, j -= 2) {
	    bcopy(&src[i*width], &dst[j*width], whalf);
	    revcopy(&src[i*width], &dst[j*width+width-1], whalf);
	}
    } else {
	for (i = 0, j = dstart; i < cnt; i++, j -= 2) {
	    bcopy(&src[i*width+whalf], &dst[j*width+whalf], whalf);
	    revcopy(&src[i*width+whalf], &dst[j*width+whalf-1], whalf);
	}
    }
}

/* Change GL color map to display IndigoVideo RGB8 data */
static void
makevideomap(void)
{
    int r, g, b;
    for (r=0; r<8; r++) {
	for (b=0; b<4; b++) {
	    for (g=0; g<8; g++) {
		mapcolor((r<<5)|(b<<3)|g,
		     (r<<5)|(r<<2)|(r>>1),
		     (g<<5)|(g<<2)|(g>>1),
		     (b<<6)|(b<<4)|(b<<2)|b);
	    }
	}
    }   
    gflush();
}
