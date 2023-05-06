/*
 *   hollowcube.c:
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
 *	Warning: works on GT, GTX, and VGX models only (requires zdraw)
 *               THIS PROGRAM DOES NOT WORK ON PERSONAL IRIS MACHINES.
 *
 *	Important notes:
 *
 *	    1.	The zbuffer mapping (specified by lsetdepth) is reversed
 *		from the default, and reduced by one bit from the full
 *		range.  The MSB is then used as a writemask, because when
 *		it is set the pixel is nearer (i.e. has greater depth value)
 *		than any drawable pixel value.
 *
 *	    2.	The VGX has a signed 24-bit zbuffer, and the GT/GTX have
 *		unsigned 24-bit zbuffers.  In order for this code to work
 *		on both machine types, bit 23 is always left with value
 *		zero.  Bit 22 is used as the mask bit, and bits 0 through
 *		21 are depth bits.
 *
 *	    3.	The VGX has support for hollow polygons, so the hack
 *		zbuffer code in this program is really required only for
 *		GT/GTX machines.  You might try rewriting this code for
 *		VGX operation only.
 *
 *	    4.  The RealityEngine can render hollowpolygons in using 
 *		displacepixel(). This demo breaks on the RealityEngine's
 *		32-bit zbuffer. Please see examples/realityengine/hidden.c
 *
 *	    5.	All polygons are first drawn hollow, then (conditionally)
 *		filled with background color.  Hence the zfunction must
 *		be GREATER, not GEQUAL (so that lines that are not hidden
 *		are not erased).  The code can be rewritten to first fill,
 *		then outline, in which case the zfunction should be GEQUAL
 *		(so that lines that are not hidden are drawn).
 *
 *	    6.	The code is sequenced so that the zbuffer is never enabled
 *		while zdraw is TRUE.  VGX machines enforce this rule, and
 *		therefore will fail if this sequencing is not maintained.
 *
 *          7.  There is a "for VGX eyes only" version of this program 
 *              that lives in ~4Dgifts/examples/vfx/stencil/hollowcube-vgx.c.
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
    char  str[50];

    /* check for required capabilities */
    if (getgdesc(GD_ZDRAW_GEOM) != 1) {
	fprintf(stderr,"zdraw capability required and not present.  abort\n");
	exit(1);
    }

    gversion(str);
    if(strncmp(str,"GL4DRE",6) == 0) {
	 fprintf(stderr,"Please run 4Dgifts/examples/realityengine/hidden instead of hollowcube.\n");
	 exit(1);
    }
    /* initialize graphics */
    winopen("hollow");
    RGBmode();
    doublebuffer();
    gconfig();
    subpixel(TRUE);
    mmode(MVIEWING);
    perspective(900,1.0,1.0,5.0);
    translate(0.0,0.0,-2.2);
    lsetdepth(0x3fffff,0);
    zfunction(ZF_GREATER);
    zbuffer(TRUE);
    qdevice(ESCKEY);
    qdevice(MIDDLEMOUSE);

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
	for (i=0; i<MAXQUAD; i++)
	    drawhollow(&quads[i],0xffffff);

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

drawoutlined(quad)
Quad *quad;
{
    /* outline the polygon */
    bgnclosedline();
    v3f(&(quad->v[0]));
    v3f(&(quad->v[1]));
    v3f(&(quad->v[2]));
    v3f(&(quad->v[3]));
    endclosedline();
}

#define ZMASK 0x400000
#define ZVAL_ENABLE 0
#define ZVAL_DISABLE 0x400000

drawhollow(quad,c)
Quad *quad;
long c;
{
    /*
     *  outline the polygon filling pixels just as they would be had the
     *  polygon been filled (i.e. same color and depth values).
     */

    /*
     *	disable all the pixels in the polygon.  Do this by changing
     *	a single bit in the Z-buffer.
     */
    zbuffer(FALSE);
    backbuffer(FALSE);
    zdraw(TRUE);
    wmpack(ZMASK);
    cpack(ZVAL_DISABLE);
    drawfilled(quad);

    /*
     *	enable only the pixels on the outline of the polygon
     *	for filling.  Do this by changing a single Z-buffer bit back.
     */
    cpack(ZVAL_ENABLE);
    linewidth(2);
    drawoutlined(quad);
    linewidth(1);

    /*
     *	fill the polygon using the passed color value
     */
    zdraw(FALSE);
    backbuffer(TRUE);
    zbuffer(TRUE);
    zfunction(ZF_GREATER);
    wmpack(0xffffffff);
    cpack(c);
    drawfilled(quad);

    /*
     *	enable all the pixels in the polygon
     */
    zbuffer(FALSE);
    backbuffer(FALSE);
    zdraw(TRUE);
    wmpack(ZMASK);
    cpack(ZVAL_ENABLE);
    drawfilled(quad);

    /*
     *	cleanup the mess we've made of the machine state
     */
    zdraw(FALSE);
    backbuffer(TRUE);
    zbuffer(TRUE);
    wmpack(0xffffffff);
}
