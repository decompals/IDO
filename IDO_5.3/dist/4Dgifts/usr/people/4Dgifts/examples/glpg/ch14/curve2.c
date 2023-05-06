#include <gl/gl.h>

#define X		0
#define Y		1
#define Z		2
#define XYZ		3

#define BEZIER		1
#define CARDINAL	2
#define BSPLINE		3

Matrix beziermatrix = {
    {-1.0,  3.0, -3.0, 1.0},
    { 3.0, -6.0,  3.0, 0.0},
    {-3.0,  3.0,  0.0, 0.0},
    { 1.0,  0.0,  0.0, 0.0} 
};

Matrix cardinalmatrix = {
    {-0.5,  1.5, -1.5,  0.5},
    { 1.0, -2.5,  2.0, -0.5},
    {-0.5,  0.0,  0.5,  0.0},
    { 0.0,  1.0,  0.0,  0.0}
};

Matrix bsplinematrix = {
    {-1.0/6.0,  3.0/6.0, -3.0/6.0, 1.0/6.0},
    { 3.0/6.0, -6.0/6.0,  3.0/6.0, 0.0},
    {-3.0/6.0,  0.0,      3.0/6.0, 0.0},
    { 1.0/6.0,  4.0/6.0,  1.0/6.0, 0.0}
};

#define NPOINTS		6

Coord geom2[NPOINTS][XYZ] = {
    {300.0,  50.0, 0.0},
    {350.0, 150.0, 0.0},
    {250.0, 300.0, 0.0},
    {100.0,  50.0, 0.0},
    { 50.0, 250.0, 0.0},
    {150.0, 350.0, 0.0}
};

main()
{
    int i;

    prefsize(400, 400);
    winopen("curve2");
    color(BLACK);
    clear();

    /* define 3 types of bases */
    defbasis(BEZIER, beziermatrix);
    defbasis(CARDINAL, cardinalmatrix);
    defbasis(BSPLINE, bsplinematrix);

    curvebasis(BEZIER);	    /* the Bezier matrix becomes the current basis */
    curveprecision(20);     /* the curves will be drawn using 20 line segs */
    color(RED);
    crvn(NPOINTS, geom2);   /* the crvn command called with a Bezier basis
                             * causes 3 separate curve segments to be drawn */

    curvebasis(CARDINAL);   /* the Cardinal matrix becomes the current basis */
    color(GREEN);
    crvn(NPOINTS, geom2);   /* the crvn command called with a Cardinal spline 
			     * basis causes a smooth curve to be drawn */

    curvebasis(BSPLINE);    /* the B-spline matrix becomes the current basis */
    color(BLUE);
    crvn(NPOINTS, geom2);   /* the crvn command called with a B-spline basis
                             * causes the smoothest curve to be drawn */

    color(WHITE);	    /* show the control points */ 
    for (i = 0; i < NPOINTS; i++)
	circf(geom2[i][X], geom2[i][Y], 1.5);

    sleep(10);
    gexit();
    return 0;
}
