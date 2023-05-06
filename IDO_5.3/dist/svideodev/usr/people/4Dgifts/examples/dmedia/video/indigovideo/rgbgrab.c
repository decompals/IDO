/*
 * Simple frame grabbing using video capture.
 *
 * To use: click on the left mouse button in either window to grab a frame
 * and display it.
 */

#include <stdio.h>
#include <stdlib.h>
#include <svideo.h>
#include <gl/gl.h>
#include <gl/device.h>

#define RGBBUFSIZE (SV_NTSC_XMAX*SV_NTSC_YMAX)
static char captureData[RGBBUFSIZE], rgbbuf[RGBBUFSIZE];

main()
{
    SVhandle V;
    long dev, live_win, still_win;
    short val;
    int w, h;

    /* Open window */
    foreground();
    prefsize(SV_NTSC_XMAX, SV_NTSC_YMAX);
    still_win = winopen("Grabbed frame");
    RGBmode();
    gconfig();
    pixmode(PM_SIZE, 8);

    prefsize(SV_NTSC_XMAX, SV_NTSC_YMAX);
    live_win = winopen("Live video");

    /* Open video device */
    if ((V = svOpenVideo()) == NULL) {
	svPerror("open");
	exit(1);
    }

    /* Associate video input with this window */
    if (svBindGLWindow(V, live_win, SV_IN_REPLACE) < 0) {
	svPerror("bindwindow");
	svCloseVideo(V);
	exit(1);
    }
    printf("Use leftmouse to grab frame\n");

    /* Event loop */
    qdevice(LEFTMOUSE);
    qdevice(WINQUIT);
    qdevice(WINSHUT);
    qdevice(ESCKEY);

    while (1) {
	dev = qread(&val);
	switch (dev) {
	case LEFTMOUSE:
	    if (val != 1)	/* button-press */
		break;
	    w = SV_NTSC_XMAX;
	    h = SV_NTSC_YMAX;
	    if (svCaptureOneFrame(V, SV_RGB8_FRAMES,
			&w, &h, (char *)captureData) < 0) {
		svPerror("captureburst");
		exit(-1);
	    }
	    svInterleaveFields(TRUE, captureData, rgbbuf, w, h);
	    winset(still_win);
	    lrectwrite(0, 0, w-1, h-1, (unsigned long *) rgbbuf);
	    winset(live_win);
	    break;

	case ESCKEY:
	    if (val)	/* exit on key up */
		break;
	case WINQUIT:
	case WINSHUT:
	    svCloseVideo(V);
	    winclose(live_win);
	    winclose(still_win);
	    exit(0);
	    break;
	}
    }
}
