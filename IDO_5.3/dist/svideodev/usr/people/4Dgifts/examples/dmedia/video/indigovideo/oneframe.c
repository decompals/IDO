/*
 * oneframe.c
 *
 * This program captures and displays one frame of video from the
 * IndigoVideo board using svCaptureOneFrame(3V). It can capture in
 * one of 4 formats: RGB-8, RGB-32, YUV or YUV with blanking buffer.
 * The frame is then displayed using the GL lrectwrite(3G) routine.
 *
 * Usage:   oneframe [options]
 *      -w width	width of frame for RGB formats
 *	-r		RGB-32 frames
 *      -b		YUV frames with blanking
 *      -y		YUV frames without blanking
 * The default format is RGB-8. To exit, hit the Esc key.
 *
 * See xrgbgrab.c for a program that uses X11 to display RGB-8 data.
 */

#ident "$Revision: 1.12 $"

#include <stdio.h>
#include <stdlib.h>
#include <svideo.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <getopt.h>

#define OPT_STR "dbrw:y?"

static void
usage(void)
{
    printf("Usage:\n");
    printf("    -d		print debug messages\n");
    printf("    -w width	width of scaled RGB frame\n");
    printf("    -r		RGB-32 frames (default is RGB-8)\n");
    printf("    -b		YUV frames with blanking (default is RGB-8)\n");
    printf("    -y		YUV frames without blanking (default is RGB-8)\n");
    printf("Type the ESC key to exit.\n");
}

/* Create and setup a GL window to display the video data */
static void
SetupWindow(int format, int w, int h)
{
    char title[50];
    char *type;
    int pixsize = 32;	/* # bits/pixel for lrectwrite(3G) */

    switch (format) {
	case SV_RGB8_FRAMES:
	    type = "RGB-8";
	    pixsize = 8;
	    break;

	case SV_RGB32_FRAMES:
	    type = "RGB-32";
	    break;

	case SV_YUV411_FRAMES:
	    type = "YUV";
	    break;

	case SV_YUV411_FRAMES_AND_BLANKING_BUFFER:
	    type = "YUV-BB";
	    break;

	default:
	    type = "?";
	    break;
    }
    sprintf(title, "Capture 1 Frame: %s", type);

    prefposition(200, 200+w-1, 100, 100+h-1);
    winopen(title);

    RGBmode();
    gconfig();
    pixmode(PM_SIZE, pixsize);
}

main(int argc, char *argv[])
{
    SVhandle V;
    svCaptureInfo ci;
    boolean debug;
    int ch, errflg;
    int bufferSize;
    long *buffer;
    long *videodata;
    long setp[2];

    debug = FALSE;
    ci.format = SV_RGB8_FRAMES;
    ci.width = 0;
    ci.height = 0;
    ci.size = 1;

    errflg = 1;
    while ((ch = getopt(argc, argv, OPT_STR)) != -1) {
        errflg = 0;
	switch (ch)	{
	    case 'd': debug = TRUE;
	              break;
	    case 'b': ci.format = SV_YUV411_FRAMES_AND_BLANKING_BUFFER;
	              break;
	    case 'r': ci.format = SV_RGB32_FRAMES;
	              break;
	    case 'y': ci.format = SV_YUV411_FRAMES;
	              break;
	    case 'w': ci.width = atoi(optarg);
	              break;
	    default:  usage();
	              exit(1);
	}
    }
    if (errflg && (argc > 1)) {
        usage();
        exit(1);
    }

    /* Open video device */
    if ((V = svOpenVideo()) == NULL) {
	svPerror("open");
	exit(1);
    }

    if (svQueryCaptureBufferSize(V, &ci, &bufferSize) < 0) {
        svPerror("svQueryCaptureBufferSize");
        exit(1);
    }
    buffer = malloc(bufferSize);

    if (svCaptureOneFrame(V, ci.format, &ci.width, &ci.height, buffer) < 0) {
        svPerror("svCaptureOneFrame");
        exit(1);
    }
    if (debug) {
        printf("captured size: %d by %d\n", ci.width, ci.height);
    }

    SetupWindow(ci.format, ci.width, ci.height);

    switch (ci.format) {
	case SV_RGB8_FRAMES:
	    videodata = malloc(bufferSize);
	    svInterleaveFields(TRUE, (char *)buffer, (char *)videodata,
				ci.width, ci.height);
	    break;

	case SV_RGB32_FRAMES:
	    videodata = buffer;
	    break;

	default:	/* YUV formats */
	    videodata = malloc(SV_PAL_XMAX
			       * (SV_PAL_YMAX + SV_PAL_BLANKING_BUFFER_SIZE)
			       * sizeof(long));
	    svYUVtoRGB(TRUE, (char *)buffer, videodata, ci.width, ci.height);
	    break;
    }

    /* Display the image */
    lrectwrite(0, 0, ci.width-1, ci.height-1, (unsigned long *)videodata);

    svCloseVideo(V);	/* Done with video */

    /* Event loop */
    qdevice(WINQUIT);
    qdevice(WINSHUT);
    qdevice(ESCKEY);	/* Exit when the user hits the escape key */

    while (1) {
	short val;
	switch (qread(&val)) {
	    case REDRAW:
		lrectwrite(0, 0, ci.width-1, ci.height-1,
				(unsigned long *)videodata);
		break;

	    case ESCKEY:
		if (val)	/* exit on key up */
		    break;
	    case WINQUIT:
	    case WINSHUT:
		exit(0);
		break;
	}
    }
}
