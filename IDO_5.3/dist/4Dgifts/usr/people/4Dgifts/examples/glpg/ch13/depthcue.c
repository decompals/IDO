#include <stdio.h>
#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>

#define RGB_BLACK   0x000000  

#define X	    0
#define Y	    1
#define Z	    2
#define XY	    2
#define XYZ	    3

#define	CORNERDIST  1.8		    /* center to furthest corner of cube */
#define EYEDIST	    3*CORNERDIST    /* center to eye */

#define NUMPOINTS   100

float points[NUMPOINTS][XYZ];
long corner[8][XYZ] = {
    {-1, -1, -1},
    {-1,  1, -1},
    { 1, -1, -1},
    { 1,  1, -1},
    {-1, -1,  1},
    {-1,  1,  1},
    { 1, -1,  1},
    { 1,  1,  1}
};
int edge[12][2] = {
    {0, 1}, {1, 3}, {3, 2}, {2, 0},
    {4, 5}, {5, 7}, {7, 6}, {6, 4},
    {0, 4}, {1, 5}, {2, 6}, {3, 7},
};

void drawcube()
{
    int i;

    for (i = 0; i < 12; i++) {
	bgnline();
	    v3i(corner[edge[i][0]]);
	    v3i(corner[edge[i][1]]);
	endline();
    }
}

void drawpoints()
{
    int i;

    bgnpoint();
    for (i = 0; i < NUMPOINTS; i++)
	v3f(points[i]);
    endpoint();
    drawcube();
}

main()
{
    long maxscreen[XY];
    Device mdev[XY];
    short mval[XY];
    float rotang[XY];
    short val;
    int i;

    if (getgdesc(GD_BITS_NORM_DBL_RED) == 0) {
	fprintf(stderr, "Double buffered RGB not available on this machine\n");
	return 1;
    }

    prefsize(400, 400);
    winopen("depthcue");
    doublebuffer();
    RGBmode();
    gconfig();
    cpack(RGB_BLACK);
    clear();
    swapbuffers();

    qdevice(ESCKEY);
    maxscreen[X] = getgdesc(GD_XPMAX) - 1;
    maxscreen[Y] = getgdesc(GD_YPMAX) - 1;
    mdev[X] = MOUSEX;
    mdev[Y] = MOUSEY;
    mmode(MVIEWING);
    window(-CORNERDIST, CORNERDIST, -CORNERDIST, CORNERDIST,
	    EYEDIST, EYEDIST + 2*CORNERDIST);
    lookat(0.0, 0.0, EYEDIST + CORNERDIST, 0.0, 0.0, 0.0, 0);    

    /* map the current machine's z range to 0x0 -> 0x7fffff */
    glcompat(GLC_ZRANGEMAP, 1);
    /* set up the mapping of screen z values to cyan intensity */
    lRGBrange(0, 15, 15, 0, 255, 255, 0x0, 0x7fffff);
    /* have screen z values control the color */
    depthcue(TRUE);

    /* generate random points */
    for (i = 0; i < NUMPOINTS; i++) {
	points[i][X] = 2.0 * (drand48() - 0.5);
	points[i][Y] = 2.0 * (drand48() - 0.5);
	points[i][Z] = 2.0 * (drand48() - 0.5);
    }

    while (!(qtest() && qread(&val) == ESCKEY && val == 0)) {
	cpack(RGB_BLACK);
	clear();
	getdev(XY, mdev, mval);
	rotang[X] = 4.0 * (mval[Y] - 0.5 * maxscreen[Y]) / maxscreen[Y];
	rotang[Y] = 4.0 * (mval[X] - 0.5 * maxscreen[X]) / maxscreen[X];
	rot(rotang[X], 'x');
	rot(rotang[Y], 'y');
	drawpoints();
	swapbuffers();
    }
    gexit();
    return 0;
}
