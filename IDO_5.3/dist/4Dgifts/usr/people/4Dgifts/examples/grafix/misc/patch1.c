/*
 *   patch1.c:
 *
 *     draws three surface patches:  RED   == Bezier
 *                                   BLUE  == B-Spline
 *                                   GREEN == Cardinal
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define X		0
#define Y		1
#define Z		2
#define XY		2
#define XYZ		3

#define	WSIZE		400 

#define BEZIER		1
#define CARDINAL	2
#define BSPLINE		3

Matrix beziermatrix = {
    { -1.0,  3.0, -3.0, 1.0 },
    {  3.0, -6.0,  3.0, 0.0 },
    { -3.0,  3.0,  0.0, 0.0 },
    {  1.0,  0.0,  0.0, 0.0 } 
};

Matrix cardinalmatrix = {
    { -0.5,  1.5, -1.5,  0.5 },
    {  1.0, -2.5,  2.0, -0.5 },
    { -0.5,  0.0,  0.5,  0.0 },
    {  0.0,  1.0,  0.0,  0.0 }
};

Matrix bsplinematrix = {
    { -1.0/6.0,  3.0/6.0, -3.0/6.0, 1.0/6.0 },
    {  3.0/6.0, -6.0/6.0,  3.0/6.0, 0.0 },
    { -3.0/6.0,  0.0,      3.0/6.0, 0.0 },
    {  1.0/6.0,  4.0/6.0,  1.0/6.0, 0.0 }
};

Matrix geom[XYZ] = {
    {{   0.0, 100.0, 200.0, 300.0 },
     {   0.0, 100.0, 200.0, 300.0 },
     { 700.0, 600.0, 500.0, 400.0 },
     { 700.0, 600.0, 500.0, 400.0 }},

    {{ 400.0, 500.0, 600.0, 700.0 },
     {   0.0, 100.0, 200.0, 300.0 },
     {   0.0, 100.0, 200.0, 300.0 },
     { 400.0, 500.0, 600.0, 700.0 }},

    {{ 100.0, 200.0, 300.0, 400.0 },
     { 100.0, 200.0, 300.0, 400.0 },
     { 100.0, 200.0, 300.0, 400.0 },
     { 100.0, 200.0, 300.0, 400.0 }},
};

void initialize();
void drawscene();

main()
{
    Device dev;
    short val;

    initialize();

    while (TRUE) {
	dev = qread(&val);
	if (dev == ESCKEY && val == 0) {
	    gexit();
	    exit(0);
	    /* NOTREACHED */
	} else if (dev == REDRAW) {
	    drawscene();
	}
    }
    /* NOTREACHED */
}

void initialize()
{
    long org[XY];
    short gid;

    /* center window on screen and don't allow resizing */
    org[X] = (getgdesc(GD_XPMAX) - WSIZE)/2;
    org[Y] = (getgdesc(GD_YPMAX) - WSIZE)/2;
    prefposition(org[X], org[X] + WSIZE - 1, org[Y], org[Y] + WSIZE - 1);
    gid = winopen("patch1");
    prefsize(WSIZE, WSIZE);
    winconstraints();

    qdevice(ESCKEY);
    qenter(REDRAW, gid);

    ortho(-100.0, 800.0, -100.0, 800.0, -800.0, 100.0);

    /* define 3 types of bases */
    defbasis(BEZIER, beziermatrix);
    defbasis(CARDINAL, cardinalmatrix);
    defbasis(BSPLINE, bsplinematrix);

    /*
     * seven curve segments will be drawn in the u direction and four in the
     * v direction 
     */
    patchcurves(4, 7);

    /*
     * the curve segments in u direction will consist of 20 line segments
     * (the lowest multiple of vcurves greater than usegments) and the curve
     * segments in the v direction will consist of 21 line segments (the
     * lowest multiple of ucurves greater than vsegments) 
     */
    patchprecision(20, 20);
}

void drawscene()
{
    int i, j;

    color(BLACK);
    clear();
				
    /* the patch is drawn based on the sixteen specified control points */
    patchbasis(BEZIER, BEZIER);
    color(RED);
    patch(geom[X], geom[Y], geom[Z]);

    /*
     * another patch is drawn using the same control points but a different
     * basis 
     */
    patchbasis(CARDINAL, CARDINAL);
    color(GREEN);
    patch(geom[X], geom[Y], geom[Z]);

    /* a third patch is drawn */
    patchbasis(BSPLINE, BSPLINE);
    color(BLUE);
    patch(geom[X], geom[Y], geom[Z]);

    /* show the control points */ 
    color(WHITE);
    for (i = 0; i < 4; i++) {
	for (j = 0; j < 4; j++) {
	    pushmatrix();
		translate(geom[X][i][j], geom[Y][i][j], geom[Z][i][j]);
		circf(0.0, 0.0, 3.0);
	    popmatrix();
	}
    }
}
