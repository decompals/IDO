/*
 *   hollowcube-vgx.c:
 *
 *   This is an implementation of outlining polygons based upon the
 *   techniques discussed in the "IRIS Universe", Issue Number Eleven,
 *   "The Hidden Charms Of Z-Buffer."
 *
 *	Kurt Akeley
 *	3 October 1990
 *
 *	Use hollow polygons to draw an outlined cube.
 *	Depress the middle mouse button to remove hidden lines
 *	    (by filling the cube's faces with background color).
 *	Depress the escape key to exit.
 *
 *	Warning: this program WORKS ON VGX MODELS ONLY 
 *      	 (it requires POLYMODE(3G))
 *
 *	Important notes:
 *
 *	    1.	The stencil bitplanes are cleared only once, because
 *		they are used only to draw hollow polygons, and each
 *		polygon drawn leaves the stencil buffer in the state
 *		that it found it.
 *
 *	    2.	All polygons are first drawn hollow, then (conditionally)
 *		filled with background color.  Hence the zfunction must
 *		be GREATER, not GEQUAL (so that lines that are not hidden
 *		are not erased).  The code can be rewritten to first fill,
 *		then outline, in which case the zfunction should be GEQUAL
 *		(so that lines that are not hidden are drawn).
 *
 *	    3.	The two stencil() calls should not be required, but are
 *		in release 3.3.0.  I'm not sure whether the 3.3.2 release
 *		eliminates this requirement.
 *
 *          4.  There is a "non-VGX" version of this program that lives in
 *              ~4Dgifts/examples/grafix/hollowcube.c.
 */

#include <gl/gl.h>
#include <gl/device.h>
#include <stdio.h>

#define MAXQUAD 6

typedef struct {
    float x,y,z,w;
} Vertex;

typedef struct {
    Vertex v[4];
} Quad;

/* data to define the six faces of a cube */
Quad quads[MAXQUAD] = {
    {{{0,0,0,1}, {1,0,0,1}, {1,1,0,1}, {0,1,0,1}} },
    {{{0,0,1,1}, {1,0,1,1}, {1,1,1,1}, {0,1,1,1}} },
    {{{0,0,0,1}, {1,0,0,1}, {1,0,1,1}, {0,0,1,1}} },
    {{{0,1,0,1}, {1,1,0,1}, {1,1,1,1}, {0,1,1,1}} },
    {{{0,0,0,1}, {0,0,1,1}, {0,1,1,1}, {0,1,0,1}} },
    {{{1,0,0,1}, {1,0,1,1}, {1,1,1,1}, {1,1,0,1}} },
};

main(argc,argv)
char *argv[];
{
    int i;
    short dev,val;

    /* check for required capabilities */
    if (getgdesc(GD_BITS_STENCIL) < 1) {
	fprintf(stderr,"stencil capability required and not present.  abort\n");
	exit(1);
    }

    /* initialize graphics */
    winopen("hollow.vgx");
    RGBmode();
    doublebuffer();
    stensize(1);
    gconfig();
    subpixel(TRUE);
    mmode(MVIEWING);
    perspective(900,1.0,1.0,5.0);
    translate(0.0,0.0,-2.2);
    zbuffer(TRUE);
    lsetdepth(0x7fffff,0);
    zfunction(ZF_GREATER);
    qdevice(ESCKEY);
    qdevice(MIDDLEMOUSE);
    sclear(0);

    /* loop drawing the cube */
    while (1) {

	/* handle events */
	while (qtest()) {
	    dev = qread(&val);
	    switch(dev) {
		case REDRAW:
		    reshapeviewport();
		    break;
		case ESCKEY:
		    exit(0);
	    }
	}
	pushmatrix();

	/* track the mouse */
	rot(0.5 * getvaluator(MOUSEX),'y');
	rot(0.5 * getvaluator(MOUSEY),'z');
	translate(-0.5,-0.5,-0.5);
	czclear(0,0);

	/* draw the lines as hollow polygons */
	polymode(PYM_HOLLOW);
	stencil(TRUE,1,SF_EQUAL,1,ST_KEEP,ST_KEEP,ST_KEEP);
	cpack(0xffffff);
	for (i=0; i<MAXQUAD; i++)
	    drawfilled(&quads[i]);
	polymode(PYM_FILL);
	stencil(FALSE,0,0,0,0,0,0);

	/* erase hidden lines with filled polygons */
	if (getbutton(MIDDLEMOUSE)) {
	    cpack(0x000000);
	    for (i=0; i<MAXQUAD; i++)
		drawfilled(&quads[i]);
	}
	swapbuffers();
	popmatrix();
    }
}

drawfilled(quad)
Quad *quad;
{
    /* fill the polygon */
    bgnpolygon();
    v3f(&(quad->v[0]));
    v3f(&(quad->v[1]));
    v3f(&(quad->v[2]));
    v3f(&(quad->v[3]));
    endpolygon();
}
