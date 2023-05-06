#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define RGB_BLACK   0x000000  
#define RGB_GREEN   0x00ff00

#define HOLESIZE    32
#define HOLESEP	    (HOLESIZE/2)

float v[8][3] = {
    {-1.0, -1.0, -1.0},
    {-1.0, -1.0,  1.0},
    {-1.0,  1.0,  1.0},
    {-1.0,  1.0, -1.0},
    { 1.0, -1.0, -1.0},
    { 1.0, -1.0,  1.0},
    { 1.0,  1.0,  1.0},
    { 1.0,  1.0, -1.0},
};
int face[6][4] = {
    {0, 1, 2, 3},
    {3, 2, 6, 7},
    {7, 6, 5, 4},
    {4, 5, 1, 0},
    {1, 2, 6, 5},
    {0, 4, 7, 3},
};
unsigned long facecolor[6] = {
    0xff0000,			/* blue */
    0x0000ff,			/* red */
    0x00ffff,			/* yellow */
    0xffff00,			/* cyan */
    0xff00ff,	    		/* magenta */
    0xffffff,			/* white */
}; 

void drawcube()
{
    int i;

    for (i = 0; i < 6; i++) {
	cpack(facecolor[i]);
	bgnpolygon();
	    v3f(v[face[i][0]]);
	    v3f(v[face[i][1]]);
	    v3f(v[face[i][2]]);
	    v3f(v[face[i][3]]);
	endpolygon();
    }
}

main(argc, argv)
int argc;
char *argv[];
{
    int i, j;
    Angle xang, yang;
    short val;
    Boolean use_geom;
    unsigned long holez[HOLESIZE*HOLESIZE];

    if (getgdesc(GD_BITS_NORM_DBL_RED) == 0) {
	fprintf(stderr, "Double buffered RGB not available on this machine\n");
	return 1;
    }
    if (getgdesc(GD_BITS_NORM_ZBUFFER) == 0) {
	fprintf(stderr, "Z-buffer not available on this machine\n");
	return 1;
    }
    if (getgdesc(GD_ZDRAW_GEOM) == 0 && getgdesc(GD_ZDRAW_PIXELS) == 0) {
	fprintf(stderr, "Z-buffer drawing not available on this machine\n");
	return 1;
    }
    prefsize(400, 400);
    winopen("zdraw");
    RGBmode();
    doublebuffer();
    gconfig();
    qdevice(ESCKEY);
    mmode(MVIEWING);
    ortho(-2.0, 2.0, -2.0, 2.0, -2.0, 2.0);
    zbuffer(TRUE);
    zclear();
    use_geom = getgdesc(GD_ZDRAW_GEOM) == 1;
    if (!use_geom) {
        holez[0] = getgdesc(GD_ZMAX);
	for (i = 1; i < HOLESIZE*HOLESIZE; i++)
	    holez[i] = holez[0];
    }

    /* draw the green wall once */
    cpack(RGB_GREEN);
    frontbuffer(TRUE);
    pushmatrix();
	translate(0.0, 0.0, 1.9);
	rectf(-2.00, -2.00, 2.00, 2.00);
    popmatrix();
    frontbuffer(FALSE);

    xang = yang = 0;
    while (!(qtest() && qread(&val) == ESCKEY && val == 0)) {
	/* create the holes in the green wall */
	zbuffer(FALSE);
	zdraw(TRUE);
	backbuffer(FALSE);
	if (use_geom) {
    	    ortho2(-0.5, 399.5, -0.5, 399.5);
	    cpack(getgdesc(GD_ZMAX));
	}
	for (i = 100; i <= 300; i += 50) {
	    for (j = 100; j <= 300; j += 50) {
		if (use_geom)
		    rectf(i, j, i + HOLESIZE - 1, j + HOLESIZE - 1);
		else
		    lrectwrite(i, j, i +HOLESIZE - 1, j + HOLESIZE - 1, holez);
	    }
	}
	if (use_geom)
	    ortho(-2.0, 2.0, -2.0, 2.0, -2.0, 2.0);
	zdraw(FALSE);
	backbuffer(TRUE);
	zbuffer(TRUE);

        /* z-buffered clear to background color and depth */
	cpack(RGB_BLACK);
	pushmatrix();
	    translate(0.0, 0.0, -1.9);
	    rectf(-2.00, -2.00, 2.00, 2.00);
	popmatrix();

	/* draw the outside scene */
	pushmatrix();
	    rotate(xang, 'x');
	    rotate(yang, 'y');
	    drawcube();
	popmatrix();
	swapbuffers();

	/* update the rotation angles for next time through */
	xang += 11;
	yang += 17;
	if (xang > 3600)
	    xang -= 3600;
	if (yang > 3600)
	    yang -= 3600;
    }
    gexit();
    return 0;
}
