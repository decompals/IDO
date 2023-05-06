/*
 * Draw an antialiased circle using the accumulation buffer.
 * Disable antialiasing when the left mouse button is depressed.
 * Disable subpixel positioning when the middle mouse button is depressed.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define WINSIZE 100
#define SAMPLES 3
#define DELTA (2.0 / (WINSIZE * SAMPLES))

main()
{
    long x, y;
    short val;

    if (getgdesc(GD_BITS_ACBUF) == 0) {
	fprintf(stderr, "accumulation buffer not available\n");
	return 1;
    }
    prefsize(WINSIZE, WINSIZE);
    winopen("acbuf.rgb");
    mmode(MVIEWING);
    glcompat(GLC_OLDPOLYGON,0);		/* point sample the circle */
    doublebuffer();
    RGBmode();
    acsize(16);
    gconfig();
    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);

    while (!(qtest() && qread(&val) == ESCKEY && val == 0)) {
	subpixel(getbutton(MIDDLEMOUSE) ? FALSE : TRUE);
	if (getbutton(LEFTMOUSE)) {
	    drawcirc(0.0,0.0);
	} else {
	    acbuf(AC_CLEAR,0.0);
	    for (x=0; x < SAMPLES; x++) {
		for (y=0; y < SAMPLES; y++) {
		    drawcirc((x-(SAMPLES/2))*DELTA,(y-(SAMPLES/2))*DELTA);
		    acbuf(AC_ACCUMULATE,1.0);
		}
	    }
	    acbuf(AC_RETURN,1.0/(SAMPLES*SAMPLES));
	}
	swapbuffers();
    }
    gexit();
    return 0;
}

drawcirc(xdelta,ydelta)
float xdelta,ydelta;
{
    ortho2(-1.0 + xdelta, 1.0 + xdelta, -1.0 + ydelta, 1.0 + ydelta);
    cpack(0);
    clear();
    cpack(0xffffffff);
    circf(0.0,0.0,0.8);
}
