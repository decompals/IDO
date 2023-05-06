/*
 * Drag a string of RGB antialiased points with the cursor.
 * Change from a merge-blend to an accumulate-blend when the left
 *   mouse button is depressed.
 * Use the "smoother" antialiasing sampling algorithm when the middle
 *   mouse button is depressed.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define WINSIZE     400
#define MAXPOINT    25

Device devs[2] = {MOUSEX,MOUSEY};

main()
{
    short val, vals[2];
    long i, xorg, yorg;
    float vert[2], x, y, interp;

    if (getgdesc(GD_PNTSMOOTH_RGB) == 0) {
	fprintf(stderr, "RGB mode point antialiasing not available\n");
	return 1;
    }
    prefsize(WINSIZE, WINSIZE);
    winopen("pntsmooth.rgb");
    mmode(MVIEWING);
    ortho2(-0.5, WINSIZE-0.5, -0.5, WINSIZE-0.5);
    doublebuffer();
    RGBmode();
    gconfig();
    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    getorigin(&xorg, &yorg);
    subpixel(TRUE);

    while (!(qtest() && qread(&val) == ESCKEY && val == 0)) {
	cpack(0);
	clear();
	getdev(2,devs,vals);
	x = vals[0] - xorg;
	y = vals[1] - yorg;
	cpack(0xffffffff);
	blendfunction(BF_SA,getbutton(LEFTMOUSE) ? BF_ONE : BF_MSA);
	pntsmooth(getbutton(MIDDLEMOUSE) ? (SMP_ON | SMP_SMOOTHER) : SMP_ON);
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
