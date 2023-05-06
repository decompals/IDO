/*
 * curve3.c:
 *
 *   Draws a Bezier curve segment using curveit().  Cardinal spline and
 *   B-spline curve segments could be drawn using a similar sequence of 
 *   commands -- only the basis matrix would be different.
 */

#include <gl/gl.h>
#include <gl/device.h>

#define X		0
#define Y		1
#define XY		2
#define XYZ		3

#define	WSIZE		400 

Matrix beziermatrix = {
    { -1.0,  3.0, -3.0, 1.0 },
    {  3.0, -6.0,  3.0, 0.0 },
    { -3.0,  3.0,  0.0, 0.0 },
    {  1.0,  0.0,  0.0, 0.0 } 
};

Matrix geom3 = {
    { 100.0, 200.0, 0.0, 1.0 },
    { 200.0, 300.0, 0.0, 1.0 },
    { 200.0, 100.0, 0.0, 1.0 },
    { 300.0, 200.0, 0.0, 1.0 } 
};

Matrix precisionmatrix = { 
    { 6.0/8000.0, 0.0,	     0.0,      0.0 },
    { 6.0/8000.0, 2.0/400.0, 0.0,      0.0 },
    { 1.0/8000.0, 1.0/400.0, 1.0/20.0, 0.0 },
    { 0.0,	  0.0,	     0.0,      1.0 }
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
    gid = winopen("curve3");
    prefsize(WSIZE, WSIZE);
    winconstraints();

    qdevice(ESCKEY);
    qenter(REDRAW, gid);
}

void drawscene() 
{
    int i;

    color(BLACK);
    clear();

    pushmatrix();		/* the current transformation (ortho2) 
				 * matrix on the matrix stack is saved */
    multmatrix(geom3);          /* the product of the current transformation
                                 * matrix and the matrix containing the 
				 * control points becomes the new current 
				 * transformation matrix */
    multmatrix(beziermatrix);   /* the product of the basis matrix and the
                                 * current transformation matrix becomes the 
				 * new current transformation matrix */
    multmatrix(precisionmatrix);/* the product of the precision matrix
                                 * and the current transformation matrix 
				 * becomes the new current transformation 
				 * matrix */
    move(0.0, 0.0, 0.0);        /* this command must be issued so that the 
                                 * correct first point is generated by the 
				 * curveit command */
    color(RED);
    curveit(20);                /* a curve consisting of 20 line segments 
				 * is drawn */
    popmatrix();                /* the original transformation matrix is 
				 * restored */

    color(WHITE);		/* show the control points */ 
    for (i = 0; i < 4; i++)
	circf(geom3[i][X], geom3[i][Y], 1.5);
}