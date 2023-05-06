/****************************************************************************
Copyright 1991 by Silicon Graphics Incorporated, Mountain View, California.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Silicon Graphics not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

SILICON GRAPHICS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
EVENT SHALL SILICON GRAPHICS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

****************************************************************************/

/*
 *	Kurt Akeley
 *	November 1992
 *
 *	Use displacepolygon to draw hidden-line images.  Displacepolygon
 *	    shifts the z values of polygons an amount that is
 *	    proportional to their slope in screen z.  This keeps
 *	    the lines, which are drawn without displacement, from
 *	    interacting with their respective polygons, and
 *	    thus eliminates line dropouts.
 *
 *	WARNING: displacepolygon is currently available only on
 *	         RealityEngine graphics.
 *
 *	Depress the middle mouse button to remove hidden lines
 *	    (by filling the cubes' faces with background color).
 *
 *	Depress the escape key to exit.
 */

#include <gl/gl.h>
#include <gl/device.h>
#include <stdio.h>

#define MAXQUAD 6

typedef float Vertex[4];

typedef Vertex Quad[4];

/* data to define the six faces of a cube */
Quad quads[MAXQUAD] = {
    0,0,0,1, 1,0,0,1, 1,1,0,1, 0,1,0,1,
    0,0,1,1, 1,0,1,1, 1,1,1,1, 0,1,1,1,
    0,0,0,1, 1,0,0,1, 1,0,1,1, 0,0,1,1,
    0,1,0,1, 1,1,0,1, 1,1,1,1, 0,1,1,1,
    0,0,0,1, 0,0,1,1, 0,1,1,1, 0,1,0,1,
    1,0,0,1, 1,0,1,1, 1,1,1,1, 1,1,0,1
};

void fill(Quad quad);
void outline(Quad quad);
void drawhidden(Quad quad, long c, int fill);

int dimension = 5;

main(int argc, char** argv) {
    int i;
    int middlemouse;
    short dev,val;
    int x,y,z;

    /* check for required capabilities */
    if (getgdesc(GD_TEXTURE_3D) < 1) {
	fprintf(stderr,"RealityEngine required.  Abort\n");
	exit(1);
    }

    /* read arguments */
    if (argc > 1)
	dimension = atoi(argv[1]);
    printf("dimension = %d\n", dimension);

    /* initialize graphics */
    winopen("hidden");
    RGBmode();
    doublebuffer();
    gconfig();
    subpixel(TRUE);
    mmode(MVIEWING);
    perspective(900,1.0,1.0,5.0);
    translate(0.0,0.0,-2.2);
    lsetdepth(getgdesc(GD_ZMAX),0);	/* I like clearing z to zero */
    zfunction(ZF_GEQUAL);
    zbuffer(TRUE);

    /* Turn on AA lines */
    linesmooth(SML_ON | SML_SMOOTHER);
    blendfunction(BF_SA, BF_MSA);

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
	middlemouse = getbutton(MIDDLEMOUSE);

	/* draw the lines as hidden polygons */
	czclear(0,0);
	translate(-0.5,-0.5,-0.5);
	scale(1.0/dimension,1.0/dimension,1.0/dimension);
	for (z=0; z < dimension; z++) {
	    for (y=0; y < dimension; y++) {
		for (x=0; x < dimension; x++) {
		    pushmatrix();
		    translate(x,y,z);
		    scale(0.8,0.8,0.8);
		    for (i=0; i<MAXQUAD; i++)
			drawhidden(quads[i],0xffffff,middlemouse);
		    popmatrix();
		}
	    }
	}
	swapbuffers();
	popmatrix();
    }
}

void fill(Quad quad) {
    /* fill the polygon */
    bgnpolygon();
    v3f(quad[0]);
    v3f(quad[1]);
    v3f(quad[2]);
    v3f(quad[3]);
    endpolygon();
}

void outline(Quad quad) {
    /* outline the polygon */
    bgnclosedline();
    v3f(quad[0]);
    v3f(quad[1]);
    v3f(quad[2]);
    v3f(quad[3]);
    endclosedline();
}

void drawhidden(Quad quad, long c, int dofill) {
    /* draw the outline using color c, fill the interior with black */
    cpack(c);
    outline(quad);
    if (dofill) {
	cpack(0);
	displacepolygon(-1); /* scale factor is negative because 0 is far */
	fill(quad);
	displacepolygon(0);
    }
}

