/*
 *	vgrab.c
 *      Grab YUV frames, save as SGI RGB images
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/image.h>
#include <gl/device.h>
#include <svideo.h>

#define GRABFILE	"out.rgb"
#define RGBBUFSIZE	(SV_PAL_XMAX*SV_PAL_YMAX*sizeof(long))
static char rgbbuf[RGBBUFSIZE];

/*
 * Dump rgb data to image file
 */
void
dumpImage(char *data, int xsize, int ysize)
{
    IMAGE *image;
    short rbuf[SV_PAL_XMAX];
    short gbuf[SV_PAL_XMAX];
    short bbuf[SV_PAL_XMAX];
    int x, y, z;
    image = iopen(GRABFILE, "w", RLE(1), 3, xsize, ysize, 3);
    for (y=0;y<ysize;y++) {
	for(x=0;x<xsize;x++) {
	    bbuf[x] = *(data+1);
	    gbuf[x] = *(data+2);
	    rbuf[x] = *(data+3);
	    data += 4;
	}
	putrow(image, rbuf, y, 0);
	putrow(image, gbuf, y, 1);
	putrow(image, bbuf, y, 2);
	
    }
    iclose(image);
}

main(int argc, char **argv)
{
    short val;
    long livewin, stillwin, x, y;
    int width, height;
    SVhandle V;
    long param[2];
    int videoon = 1;

    /* Open video device */
    if ((V = svOpenVideo()) == NULL) {
	svPerror("open");
	exit(1);
    }

    /* Determine window size based on signal standard */
    param[0] = SV_BROADCAST;
    svGetParam(V, param, 2);
    if (param[1] == SV_PAL) {
	width = SV_PAL_XMAX;
	height = SV_PAL_YMAX;
    } else {
	width = SV_NTSC_XMAX;
	height = SV_NTSC_YMAX;
    }

    /* Open windows */
    foreground();
    prefsize(width, height);
    stillwin = winopen("Grabbed frame");
    RGBmode();
    gconfig();

    /* Set video window background to black */
    cpack(0x0);
    clear();
    maxsize(width, height);
    keepaspect(width, height);
    stepunit(8, 6);
    livewin = winopen("video in");
    RGBmode();
    gconfig();

    getsize(&x, &y);
    svSetSize(V, x, y);

    /* Associate video input with livewin */
    if (svBindGLWindow(V, livewin, SV_IN_REPLACE) < 0) {
	svPerror("bindwindow");
	exit(1);
    }

    printf("Click on left mouse button to grab frame\n");
    qdevice(LEFTMOUSE);
    qdevice(WINQUIT);
    qdevice(WINSHUT);
    qdevice(ESCKEY);

    while (1) {
	switch (qread(&val)) {
	    case LEFTMOUSE:
		if (val != 1)
		    break;

		svCaptureOneFrame(V, SV_RGB32_FRAMES, &width, &height, rgbbuf);
		winset(stillwin);
		lrectwrite(0, 0, width-1, height-1, (unsigned long *) rgbbuf);
		winset(livewin);
		if (svSetSize(V, x, y) < 0) {
		    svPerror("setsize");
		    exit(1);
		}
		/* Re-bind window to re-scale output */
		if (svBindGLWindow(V, livewin, SV_IN_REPLACE) < 0) {
		    svPerror("bindwindow");
		    exit(1);
		}		
		dumpImage(rgbbuf, width, height);
		printf("saved image to file %s\n", GRABFILE);
		break;

	    case REDRAW:
		reshapeviewport();
		getsize(&x, &y);
		svSetSize(V, x, y);
		/* Re-bind window to re-scale output */
		if (svBindGLWindow(V, livewin, SV_IN_REPLACE) < 0) {
		    svPerror("bindwindow");
		    exit(1);
		}
		break;

	    case ESCKEY:
		if (val)	/* exit on key up */
		    break;
	    case WINQUIT:
	    case WINSHUT:
		winclose(stillwin);
		winclose(livewin);
		svCloseVideo(V);
		exit(0);
		break;
	}
    }
}
