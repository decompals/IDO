/*
 * Drag a string of color map antialiased points with the cursor.
 * Disable antialiasing while the left mouse button is depressed.
 * Disable subpixel positioning while the middle mouse button is depressed.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define WINSIZE     400
#define RAMPBASE    64	    /* avoid the first 64 colors */
#define RAMPSIZE    16
#define RAMPSTEP    (255 / (RAMPSIZE-1))
#define MAXPOINT    25

Device devs[2] = {MOUSEX,MOUSEY};

main()
{
    short val, vals[2];
    long i, xorg, yorg;
    float vert[2], x, y, interp;

    if (getgdesc(GD_PNTSMOOTH_CMODE) == 0) {
	fprintf(stderr, "Color map mode point antialiasing not available\n");
	return 1;
    }
    if (getgdesc(GD_BITS_NORM_DBL_CMODE) < 8) {
	fprintf(stderr, "Need 8 bitplanes in doublebuffer color map mode\n");
	return 1;
    }
    prefsize(WINSIZE, WINSIZE);
    winopen("pntsmooth.index");
    mmode(MVIEWING);
    ortho2(-0.5, WINSIZE-0.5, -0.5, WINSIZE-0.5);
    doublebuffer();
    gconfig();
    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    getorigin(&xorg, &yorg);
    for (i = 0; i < RAMPSIZE; i++)
	mapcolor(i + RAMPBASE, i * RAMPSTEP, i * RAMPSTEP, i * RAMPSTEP);

    while (!(qtest() && qread(&val) == ESCKEY && val == 0)) {
	color(RAMPBASE);
	clear();
	getdev(2,devs,vals);
	x = vals[0] - xorg;
	y = vals[1] - yorg;
	pntsmooth(getbutton(LEFTMOUSE) ? SMP_OFF : SMP_ON);
	subpixel(getbutton(MIDDLEMOUSE) ? FALSE : TRUE);
	color(RAMPBASE+RAMPSIZE-1);
	bgnpoint();
	for (i=0; i<=MAXPOINT; i++) {
	    interp = (float)i / (float)MAXPOINT;
	    vert[0] = 100.0 * interp + x * (1.0 - interp);
	    vert[1] = 100.0 * interp + y * (1.0 - interp);
	    v2f(vert);
	}
	endpoint();
	swapbuffers();
    }
    gexit();
    return 0;
}
