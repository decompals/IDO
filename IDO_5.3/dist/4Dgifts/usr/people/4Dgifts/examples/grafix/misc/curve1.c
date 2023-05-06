/*
 * curve1.c:
 *
 *    Draws 3 curve segments ("horizontal" one is Bezier, "vertical" one is
 *    Cardinal, and "diagonal" one is B-spline).  All use the same set of 4
 *    control points, contained in geom1.  Before crv (or rcrv) is called, 
 *    a basis and precision matrix must be defined.
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

Coord geom1[4][XYZ] = {
    { 100.0, 200.0, 0.0 },
    { 200.0, 300.0, 0.0 },
    { 200.0, 100.0, 0.0 },
    { 300.0, 200.0, 0.0 }
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
    gid = winopen("curve1");
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

    curvebasis(BEZIER);     /* the Bezier matrix becomes the current basis */
    color(RED);
    crv(geom1);		    /* draw the curve based on the four control points 
			     * in geom1 */

    curvebasis(CARDINAL);   /* the Cardinal matrix becomes the current basis;
			     * note that the curveprecision does not have to 
			     * be restated unless it is to be changed */
    color(GREEN);

    curvebasis(BSPLINE);    /* the B-spline matrix becomes the current basis */
    color(BLUE);
    crv(geom1);		    /* a new curve segment is drawn */

    color(WHITE);	    /* show the control points */ 
    for (i = 0; i < 4; i++)
	circf(geom1[i][X], geom1[i][Y], 1.5);
}
