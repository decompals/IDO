/*
 * Rotate a single antialiased triangle drawn in RGB mode.
 * Disable antialiasing when the left mouse button is depressed.
 * Disable subpixel positioning when the middle mouse button is depressed.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define WINSIZE 400

float vert0[2] = {0.0,0.0};
float vert1[2] = {0.8,0.0};
float vert2[2] = {0.4,0.4};

main()
{
    short val;

    if (getgdesc(GD_POLYSMOOTH) == 0) {
	fprintf(stderr, "polygon antialiasing not available\n");
	return 1;
    }
    prefsize(WINSIZE, WINSIZE);
    winopen("polysmooth.rgb");
    mmode(MVIEWING);
    ortho2(-1.0,1.0,-1.0,1.0);
    doublebuffer();
    RGBmode();
    gconfig();
    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    blendfunction(BF_SA,BF_MSA);

    while (!(qtest() && qread(&val) == ESCKEY && val == 0)) {
	cpack(0);
	clear();
	cpack(0xffffffff);
	polysmooth(getbutton(LEFTMOUSE) ? PYSM_OFF : PYSM_ON);
	subpixel(getbutton(MIDDLEMOUSE) ? FALSE : TRUE);
	pushmatrix();
	rot(getvaluator(MOUSEX) / 25.0,'z');
	rot(getvaluator(MOUSEY) / 10.0,'x');
	bgnpolygon();
	v2f(vert0);
	v2f(vert1);
	v2f(vert2);
	endpolygon();
	popmatrix();
	swapbuffers();
    }
    gexit();
    return 0;
}
