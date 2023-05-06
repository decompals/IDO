/*
 * curve2.c: 
 *
 *    Draws 3 joined curve segments (red is Bezier, green is Cardinal, and
 *    blue is B-Spline).  geom2 contains 6 control points.  With the Bezier
 *    basis matrix, 3 sets of overlapping control points result in 3 sepa-
 *    rate curve segments.  With the Cardinal spline and B-spline matrices,
 *    the same overlapping sets of control points result in 3 joined curve
 *    segments.
 */

#include <gl/gl.h>
#include <gl/device.h>

#define X		0
#define Y		1
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

#define NPOINTS		6

Coord geom2[NPOINTS][XYZ] = { 
    { 300.0,  50.0, 0.0 },
    { 350.0, 150.0, 0.0 },
    { 250.0, 300.0, 0.0 },
    { 100.0,  50.0, 0.0 },
    {  50.0, 250.0, 0.0 },
    { 150.0, 350.0, 0.0 }
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
    gid = winopen("curve2");
    prefsize(WSIZE, WSIZE);
    winconstraints();

    qdevice(ESCKEY);
    qenter(REDRAW, gid);

    /* define 3 types of bases */
    defbasis(BEZIER, beziermatrix);
    defbasis(CARDINAL, cardinalmatrix);
    defbasis(BSPLINE, bsplinematrix);
    /* the curves will be drawn using 20 line segments */
    curveprecision(20);
}

void drawscene() 
{
    int i;

    color(BLACK);
    clear();

    curvebasis(BEZIER);      /* the Bezier matrix becomes the current basis */
    color(RED);
    crvn(NPOINTS, geom2);    /* the crvn command called with a Bezier basis
                                causes 3 separate curve segments to be drawn */

    curvebasis(CARDINAL);    /* the Cardinal matrix becomes the current basis */
    color(GREEN);
    crvn(NPOINTS, geom2);    /* the crvn command called with a Cardinal spline 
			        basis causes a smooth curve to be drawn */

    curvebasis(BSPLINE);     /* the B-spline matrix becomes the current basis */
    color(BLUE);
    crvn(NPOINTS, geom2);    /* the crvn command called with a B-spline basis
                                causes the smoothest curve to be drawn */

    color(WHITE);	     /* show the control points */ 
    for (i = 0; i < NPOINTS; i++)
	circf(geom2[i][X], geom2[i][Y], 1.5);
}
