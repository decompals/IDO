/*
 * Rotate two antialiased cubes in RGB mode.
 * Disable antialiasing by depressing the left mouse button.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define WINSIZE 400
#define SIZE	(0.2)
#define OFFSET	(0.5)
#define CUBE0	OFFSET
#define CUBE1	(-OFFSET)

float vert0[4] = {-SIZE,-SIZE, SIZE};
float vert1[4] = { SIZE,-SIZE, SIZE};
float vert2[4] = {-SIZE, SIZE, SIZE};
float vert3[4] = { SIZE, SIZE, SIZE};
float vert4[4] = {-SIZE, SIZE,-SIZE};
float vert5[4] = { SIZE, SIZE,-SIZE};
float vert6[4] = {-SIZE,-SIZE,-SIZE};
float vert7[4] = { SIZE,-SIZE,-SIZE};

float cvert0[2] = {-1.0,-1.0};
float cvert1[2] = { 1.0,-1.0};
float cvert2[2] = { 1.0, 1.0};
float cvert3[2] = {-1.0, 1.0};

main()
{
    short val;
    float xang;

    if (getgdesc(GD_POLYSMOOTH) == 0) {
	fprintf(stderr, "polygon antialiasing not available\n");
	return 1;
    }
    prefsize(WINSIZE, WINSIZE);
    winopen("polysmooth3.rgb");
    mmode(MVIEWING);
    ortho(-1.0,1.0,-1.0,1.0,-1.0,1.0);
    doublebuffer();
    RGBmode();
    gconfig();
    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
    blendfunction(BF_MIN_SA_MDA,BF_ONE);
    subpixel(TRUE);
    backface(TRUE);
    shademodel(FLAT);

    while (!(qtest() && qread(&val) == ESCKEY && val == 0)) {
	cpack(0);
	clear();
	polysmooth(getbutton(LEFTMOUSE) ? PYSM_OFF : PYSM_ON);
	pushmatrix();
	xang = getvaluator(MOUSEY) / 5.0;
	rot(xang,'x');
	rot(getvaluator(MOUSEX) / 5.0,'z');
	if (xang < 90.0) {
	    drawcube(CUBE0);
	    drawcube(CUBE1);
	} else {
	    drawcube(CUBE1);
	    drawcube(CUBE0);
	}
	popmatrix();
	drawbackground();
	swapbuffers();
    }
    gexit();
    return 0;
}

drawcube(offset)
float offset;
{
    pushmatrix();
    translate(0.0,0.0,offset);
    bgntmesh();
    v3f(vert0);
    v3f(vert1);
    cpack(0xff0000ff);
    v3f(vert2);
    v3f(vert3);
    cpack(0xff00ff00);
    v3f(vert4);
    v3f(vert5);
    cpack(0xffff0000);
    v3f(vert6);
    v3f(vert7);
    cpack(0xff00ffff);
    v3f(vert0);
    v3f(vert1);
    endtmesh();
    bgntmesh();
    v3f(vert0);
    v3f(vert2);
    cpack(0xffff00ff);
    v3f(vert6);
    v3f(vert4);
    endtmesh();
    bgntmesh();
    v3f(vert1);
    v3f(vert7);
    cpack(0xffffff00);
    v3f(vert3);
    v3f(vert5);
    endtmesh();
    popmatrix();
}

drawbackground() {
    cpack(0xffffffff);
    bgnpolygon();
    v2f(cvert0);
    v2f(cvert1);
    v2f(cvert2);
    v2f(cvert3);
    endpolygon();
}
