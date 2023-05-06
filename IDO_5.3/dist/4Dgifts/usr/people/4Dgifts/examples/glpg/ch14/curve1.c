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

Coord geom1[4][XYZ] = {
    {100.0, 200.0, 0.0},
    {200.0, 300.0, 0.0},
    {200.0, 100.0, 0.0},
    {300.0, 200.0, 0.0}
};

main()
{
    int i;

    prefsize(400, 400);
    winopen("curve1");
    color(BLACK);
    clear();

    /* define 3 types of bases */
    defbasis(BEZIER, beziermatrix);
    defbasis(CARDINAL, cardinalmatrix);
    defbasis(BSPLINE, bsplinematrix);

    curvebasis(BEZIER);     /* the Bezier matrix becomes the current basis */
    curveprecision(20);     /* the curves will be drawn using 20 line segs */
    color(RED);
    crv(geom1);		    /* draw the curve based on the four control points 
			     * in geom1 */

    curvebasis(CARDINAL);   /* the Cardinal matrix becomes the current basis;
			     * note that the curveprecision does not have to 
			     * be restated unless it is to be changed */
    color(GREEN);
    crv(geom1);		    /* a new curve segment is drawn */

    curvebasis(BSPLINE);    /* the B-spline matrix becomes the current basis */
    color(BLUE);
    crv(geom1);		    /* a new curve segment is drawn */

    color(WHITE);	    /* show the control points */ 
    for (i = 0; i < 4; i++)
	circf(geom1[i][X], geom1[i][Y], 1.5);

    sleep(10);
    gexit();
    return 0;
}
