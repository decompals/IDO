/*
 * Scalable GL Video Input Window
 */

#include <stdlib.h>
#include <svideo.h>
#include <gl/gl.h>
#include <gl/device.h>

main()
{
    short val;
    long win, dev, x, y;
    SVhandle V;

    /* Open window */
    minsize(80, 60);
    stepunit(8, 6);
    maxsize(SV_NTSC_XMAX, SV_NTSC_YMAX);
    keepaspect(SV_NTSC_XMAX, SV_NTSC_YMAX);
    win = winopen("video in");

    if ((V = svOpenVideo()) == NULL) {
	svPerror("open");
	exit(1);
    }
    getsize(&x, &y); 
    svSetSize(V, x, y);
    if (svBindGLWindow(V, win, SV_IN_REPLACE) < 0) {
	svPerror("bindwindow");
	svCloseVideo(V);
	exit(1);
    }

    /* Event loop */
    qdevice(ESCKEY);
    qdevice(WINQUIT);
    qdevice(WINSHUT);
    while (1) {
	dev = qread(&val);
	switch (dev) {
	case REDRAW:
	    getsize(&x, &y); /* may have been resized */
	    svSetSize(V, x, y);
	    /* Re-bind window to scale input */
	    if (svBindGLWindow(V, win, SV_IN_REPLACE) < 0){
		svPerror("bindwindow");
		svCloseVideo(V);
		exit(1);
	    }
	    break;

	case ESCKEY:
	case WINQUIT:
	case WINSHUT:
	    svCloseVideo(V);
	    winclose(win);
	    exit(0);
	    break;
	}
    }
}
