/*
 *	Simple GL Video Input Window
 *	Displays video on screen
 */

#include <stdlib.h>
#include <stdio.h>
#include <svideo.h>
#include <gl/gl.h>
#include <gl/device.h>

main()
{
    short val;
    long win, dev;
    SVhandle V;
    long param[4];

    /* Open NTSC-sized window */
    prefsize(SV_NTSC_XMAX, SV_NTSC_YMAX);
    foreground();
    win = winopen("video test");

    /* Open video device */
    if ((V = svOpenVideo()) == NULL) {
	svPerror("open");
	exit(1);
    }

    /* Associate video input with this window */
    if (svBindGLWindow(V, win, SV_IN_REPLACE) < 0) {
	svPerror("bindwindow");
	svCloseVideo(V);
	exit(1);
    }

    /* Paint the background when video is not displayed  */
    color(BLUE);
    clear();

    /* Event loop */
    qdevice(VIDEO);
    qdevice(ESCKEY);	/* Exit when this key is typed */
    qdevice(WINQUIT);
    qdevice(WINSHUT);

    while (1) {
	dev = qread(&val);
	switch (dev) {
	    case VIDEO:
		printf("Video device value %2d = ", val);
		if (val == SvVideoStarted) {
		    printf("Video started");
		} else if (val == SvVideoStopped) {
		    printf("Video stopped");
		} else if (val == SvVideoBusy) {
		    printf("Video busy");
		} else if (val == SvVideoPreempted) {
		    printf("Lost video");
		} else if (val == SvActiveAttribute) {
		    printf("Active attribute");
		    if (svBindGLWindow(V, win, SV_IN_REPLACE) < 0) {
			svPerror("bindwindow");
			svCloseVideo(V);
			exit(1);
		    }
		} else if (val == SvEncodingAttribute) {
		    printf("Encoding change:");
		    param[0] = SV_BROADCAST;
		    param[2] = SV_VIDEO_MODE;
		    svGetParam(V, param, 4);
		    printf(param[1] == SV_PAL ? " PAL" : " NTSC");
		    printf(param[3] == SV_COMP ? " composite" : " SVideo");
		} else if (val == SvFreezeAttribute) {
		    printf("Freeze attribute: ");
		    param[0] = SV_FREEZE;
		    svGetParam(V, param, 2);
		    printf(param[1] ? "on" : "off");
		} else if (val == SvSourceAttribute) {
		    printf("Input source change: ");
		    param[0] = SV_SOURCE;
		    svGetParam(V, param, 2);
		    printf("%d", param[1] + 1);
		} else if (val == SvParamChangeAttribute) {
		    printf("Parameter change");
		} else {
		    printf("?");
		}
		printf("\n");
		fflush(stdout);
		break;

	    case ESCKEY:
		if (val)	/* exit on key up */
		    break;
	    case WINQUIT:
	    case WINSHUT:
		svCloseVideo(V);
		exit(0);
		break;
	}
    }
}
