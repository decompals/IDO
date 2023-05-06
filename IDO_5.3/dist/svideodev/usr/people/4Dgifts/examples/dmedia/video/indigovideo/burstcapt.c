/*
 * burstcapt.c
 *
 * This program captures and displays a burst of frames from the
 * the IndigoVideo board using svCaptureBurst(3V).  Frames can be 
 * captured in RGB8, YUV411 or YUV411 with blanking buffer formats.
 * For the RGB8 format, the size of the image can be set using -w.
 *
 * Usage:   burstcapt [options]
 *      -d		print debug messages
 *      -n frames	number of frames to capture
 *      -w width	width of scaled frame
 *      -b		YUV frames with blanking (default is RGB8)
 *      -y		YUV frames without blanking (default is RGB8)
 */

#ident "$Revision: 1.16 $"

#include <stdio.h>
#include <stdlib.h>
#include <svideo.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <getopt.h>

#define OPT_STR "dbn:w:y?"

static void
usage(void)
{
    printf("Usage:\n");
    printf("    -d	print debug messages\n");
    printf("    -n frames	number of frames to capture\n");
    printf("    -w width	width of scaled frame\n");
    printf("    -b	capture YUV frames with blanking (default is RGB8)\n");
    printf("    -y	capture YUV frames without blanking (default is RGB8)\n");
    printf("Click on the left mouse button to display the next frame.\n");
    printf("Click on the middle mouse button to display all of the frames sequentially.\n");
    printf("Type the ESC key to exit.\n");
}


/* Global variables shared between main() and DisplayFrame(). */
SVhandle V;
svCaptureInfo svci;
enum { RGB, YUV, YUVBB } frameType = RGB;
long *rgb32Frame;
char *bitvec;

static void DisplayFrame(int, char *);

main(int argc, char *argv[])
{
    short val;
    boolean debug;
    int requestedwidth;
    int frameIndex, ch, errflg;
    int bitveclen, bytesToMalloc;
    int framesize;
    char *frame, *buffer;
    long param[2];

    svci.size = 8;
    debug = FALSE;
    requestedwidth = 0;

    errflg = 1;
    while ((ch = getopt(argc, argv, OPT_STR)) != -1) {
        errflg = 0;
	switch (ch)	{   
	    case 'd': debug = TRUE;
	              break;
	    case 'n': svci.size = atoi(optarg);
	              break;
	    case 'w': requestedwidth = atoi(optarg);
	              break;
	    case 'y': frameType = YUV;
	              break;
	    case 'b': frameType = YUVBB;
	              break;
	    default:  usage();
	              exit(1);
	}
    }
    if (errflg && (argc > 1)) {
        usage();
        exit(1);
    }

    /* Open video device. */
    if ((V = svOpenVideo()) == NULL) {	
	svPerror("open");
	exit(1);
    }

    /* Calculate desired width; YUV frames cannot be sized */
    if ((frameType == YUV) || (frameType == YUVBB)) {
	svci.format = SV_YUV411_FRAMES_AND_BLANKING_BUFFER;
	svci.width = 0;
    } else {
        svci.format = SV_RGB8_FRAMES;
	svci.width = requestedwidth;
    }
    svci.height = 0;
    svci.samplingrate = 0;

    /* Allocate buffers to hold video data and status bit vector */
    if (svQueryCaptureBufferSize(V, &svci, &bytesToMalloc) < 0) {
        svPerror("query capture buffer size");
	exit(1);
    }
    buffer = malloc(bytesToMalloc);
    if (frameType == RGB) {
	bitveclen = SV_BITVEC_SIZE(svci.size);
	bitvec = malloc(bitveclen);    
	bzero(bitvec, bitveclen);    
    } else {
	bitvec = NULL;
    }

    /* Capture a consecutive sequence of video frames */
    if (svCaptureBurst(V, &svci, buffer, bitvec) < 0) {
        svPerror("captureburst");
        exit(1);
    }

    /* Indicate if any frames were dropped to avoid tearing */
    if (debug) {
	u_char *p;

        printf("Burst buffer size: %d\n", bytesToMalloc);
        printf("Format: %d, #frames %d, width %d, height %d\n",
	   svci.format, svci.size, svci.width, svci.height);

	if (frameType == RGB) {
	    int i;

	    /* Recalculate since svci.size may be smaller than requested */
	    bitveclen = SV_BITVEC_SIZE(svci.size);
	    printf("Bitveclen: %d bytes\n", bitveclen);

	    printf("Captured fields:\n");
	    for (i=0; i < 2*svci.size; i += 2)
		printf("%s%s  ", 
			SV_GET_FIELD(bitvec,i) == SV_EVEN_FIELD ? "E" : "O",
			SV_GET_FIELD(bitvec,i+1) == SV_EVEN_FIELD ? "E" : "O");
	    printf("\nbitvec:\n");
	    for (i=0, p = (u_char *)bitvec; i < bitveclen/sizeof(*p); i++, p++)
		printf(" 0x%x", *p);
	    printf("\n");
	}
    }

    /* Allocate buffer to hold video data converted to 32-bit RGB format */
    framesize = svci.width * svci.height;
    rgb32Frame = malloc(framesize * sizeof(long));
    if (frameType != RGB)	/* YUV frames use twice as much space */
	framesize *= 2;
    if (frameType == YUV) {	/* display without blanking buffer */
	param[0] = SV_BROADCAST;
	if (svGetParam(V, param, 2) < 0) {
	    svPerror("getparam");
	    exit(1);
	}
	if (param[1] == SV_NTSC)
	    svci.height -= SV_NTSC_BLANKING_BUFFER_SIZE;
	else
	    svci.height -= SV_PAL_BLANKING_BUFFER_SIZE;
    }
    if (debug) 
        printf("display size: %d X %d\n", svci.width, svci.height);

    /* Open window for display of captured video frames */
    prefposition(200, 200+svci.width-1, 100, 100+svci.height-1);
    winopen("Burst Capture");
    RGBmode();
    gconfig();

    /* Display the first frame */
    frame = buffer;  
    frameIndex = 0;
    DisplayFrame(frameIndex, frame);

    /* Event loop */
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    qdevice(WINQUIT);
    qdevice(WINSHUT);
    qdevice(ESCKEY);
    while (1) {
	switch (qread(&val)) {
	    case REDRAW:
		DisplayFrame(frameIndex, frame);
		break;

	    case LEFTMOUSE:		/* Show the next frame */
		if (val != 1)	/* button-press */
		    break;
		if (++frameIndex < svci.size) {
		    frame += framesize;
		} else {
		    frame = buffer;
		    frameIndex = 0;
		}
		DisplayFrame(frameIndex, frame);
		break;

	    case MIDDLEMOUSE:		/* Loop through the frames */
		if (val != 1)
		    break;
		for (frameIndex = 0, frame = buffer;
		     frameIndex < svci.size - 1;
		     frameIndex++, frame += framesize) {

		    DisplayFrame(frameIndex, frame);
		    sginap(1);		/* sleep a bit between frames */
		}
		DisplayFrame(frameIndex, frame);	/* display last frame */
		break;

	    case ESCKEY:
		if (val)	/* exit on key up */
		    break;
	    case WINQUIT:
	    case WINSHUT:
		exit(0);
	}
    }
}

static void
DisplayFrame(int frameIndex, char *frame)
{
    char *tframe;
    char title[80];

    if (frameType == RGB) {
	/*
	 * svRGB8toRGB32(3V) and svInterleaveFields(3V) expect the
	 * buffer to contain an odd field followed by an even
	 * field.  The bit vector can be used to determine if
	 * a field has been dropped/duplicated and how to align the buffer.
	 * There are 4 cases when composing a frame from a pair of
	 * fields.  The window title below will show these cases:
	 *
	 * Typical bitvec:
	 * OE OE OE OO EO EO EO EE OE OE OE OE
	 *
	 * For NTSC:
	 *  OE	expected order
	 *  OO	1 even field dropped. To realign the buffer to OE,
	 * 	duplicate the first odd field to try to reconstruct 
	 *	1 usable frame or drop the first odd field to ignore it.
	 *	
	 *  EE	1 odd field dropped. To realign the buffer to OE,
	 *	duplicate the second even field to try to reconstruct 
	 *	1 usable frame,	or drop the second even field to ignore it.
	 *
	 * For PAL:
	 *  OE	expected order
	 *  OO	1 odd field duplicated. To realign the buffer to OE,
	 *	drop the first odd field to ignore it.
	 *  EE	1 even field duplicated. To realign the buffer to OE,
	 *	drop the first even field to ignore it.
	 *
	 * These cases assume the SV_FIELDDROP parameter has been enabled
	 * (using vpro(1V) or via svSetParam(3V)). When SV_FIELDDROP is
	 * disabled, fields are not dropped nor duplicated.
	 *
	 * How you deal with these cases depends on your application.
	 * See svtomovie.c for an example program that deals with this issue
	 * for NTSC mode.
	 */

	sprintf(title, "RGB Frame %2d (%s-%s)", frameIndex+1,
		SV_GET_FIELD(bitvec, 2*frameIndex) == SV_EVEN_FIELD ?
		    "even" : "odd",
		SV_GET_FIELD(bitvec, 2*frameIndex+1) == SV_EVEN_FIELD ?
		    "even" : "odd");
	svRGB8toRGB32(TRUE, frame, rgb32Frame, svci.width, svci.height);

    } else {
	sprintf(title, "YUV Frame %2d", frameIndex+1);
	if (frameType == YUV) {
	    svFindVisibleRegion(V, frame, (void **)&tframe, svci.width);
	} else
	    tframe = frame;
	svYUVtoRGB(TRUE, tframe, rgb32Frame, svci.width, svci.height);
    }
    wintitle(title);
    lrectwrite(0, 0, svci.width-1, svci.height-1, (unsigned long *)rgb32Frame);
}
