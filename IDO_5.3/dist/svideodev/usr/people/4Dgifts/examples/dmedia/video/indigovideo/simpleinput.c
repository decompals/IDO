#include <stdlib.h>
#include <svideo.h>
#include <gl/gl.h>
#include <gl/device.h>

main()
{
    long win, dev, params[2];
    short val;
    SVhandle V;

    /* Step 1: Open window */
    prefsize(SV_NTSC_XMAX, SV_NTSC_YMAX);
    win = winopen("video test");

    /* Step 2: Open video device */
    if ((V = svOpenVideo()) == NULL) {
	svPerror("open");
	exit(1);
    }

    /* Step 3: Set video source */
    params[0] = SV_SOURCE;
    params[1] = SV_SOURCE1;
    if (svSetParam(V, params, 2) < 0) {
	svCloseVideo(V);
	svPerror("set param");
	exit(1);
    }

    /* Step 4: Associate video input with window */
    if (svBindGLWindow(V, win, SV_IN_REPLACE) < 0) {
	svPerror("bind gl window");
	svCloseVideo(V);
	exit(1);
    }

    /* Step 5: wait for user to quit */
    qdevice(ESCKEY);
    qdevice(WINQUIT);
    qdevice(WINSHUT);
    while (1) {
	dev = qread(&val);
	switch (dev) {
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
