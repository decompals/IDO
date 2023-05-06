/*
 * Rotate a patch of antialiased triangles drawn in RGB mode.
 * Disable special polygon-blend when the left mouse button is depressed.
 * Disable subpixel positioning when the middle mouse button is depressed.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define WINSIZE 400

float vert0[2] = {0.0,0.0};
float vert1[2] = {0.0,0.4};
float vert2[2] = {0.4,0.1};
float vert3[2] = {0.4,0.3};
float vert4[2] = {0.8,0.0};
float vert5[2] = {0.8,0.4};

main()
{
    short val;

    if (getgdesc(GD_POLYSMOOTH) == 0) {
	fprintf(stderr, "polygon antialiasing not available\n");
	return 1;
    }
    prefsize(WINSIZE, WINSIZE);
    winopen("polysmooth2.rgb");
    mmode(MVIEWING);
    ortho(-1.0,1.0,-1.0,1.0,-1.0,1.0);
    doublebuffer();
    RGBmode();
    gconfig();
    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    polysmooth(PYSM_ON);
    shademodel(FLAT);

    while (!(qtest() && qread(&val) == ESCKEY && val == 0)) {
	cpack(0);
	clear();
	cpack(0xffffffff);
	if (getbutton(LEFTMOUSE))
	    blendfunction(BF_SA,BF_MSA);
	else
	    blendfunction(BF_MIN_SA_MDA,BF_ONE);
	subpixel(getbutton(MIDDLEMOUSE) ? FALSE : TRUE);
	pushmatrix();
	rot(getvaluator(MOUSEX) / 25.0,'z');
	rot(getvaluator(MOUSEY) / 10.0,'x');
	bgntmesh();
	v2f(vert0);
	v2f(vert1);
	v2f(vert2);
	v2f(vert3);
	v2f(vert4);
	v2f(vert5);
	endtmesh();
	popmatrix();
	swapbuffers();
    }
    gexit();
    return 0;
}
