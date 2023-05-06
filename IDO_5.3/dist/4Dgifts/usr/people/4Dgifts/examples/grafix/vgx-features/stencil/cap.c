
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
 *	February 1991
 *	Simple capping demonstration
 *
 *      When a polygonal surface representation of a solid object is
 *      clipped, the inside of the "solid" is revealed.  This
 *      program demonstrates a technique to "cap" the solid, so that
 *      it appears solid even when clipped.  In order to do this, it
 *      draws each object twice, once to render the visible surfaces,
 *      and again counting the number of accesses at each pixel.
 *      Finally, the clipping planes themselves are drawn, with pixels
 *      actually modified only where the pixel access count in the
 *      stencil buffer is odd (i.e. where the inside of a solid was
 *      revealed).
 *
 *      When invoked with no arguments, three internally defined models
 *      are displayed (hit the space bar to toggle between models).
 *      If an argument is given, it is expected to be the name of a
 *      flip-format .bin file.  If this file is found in either the
 *      local directory or in /usr/demos/data/models, it replaces the
 *      first internally defined model.
 *
 *      For this code to work correctly, the model must be built of
 *      proper polygonal surfaces.  Such surfaces separate space into
 *      inside and outside volumes (perhaps more than one of each).
 *      They are constructed of polygons whose adjacent edges share
 *      vertexes and do not overlap.  The martini glass (martini.bin)
 *      is an example of such a model.  None of the other models
 *      currently in the models directory is a proper solid.
 *
 *      It is acceptable for the model to include intersecting proper
 *      polygon solids, as demonstrated by the third internal model.
 *      In cases of intersection a painter's algorithm applies to the
 *      caps (i.e. the last cap drawn wins).
 *
 */

#include <stdio.h>
#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <stdio.h>
#include "flip.h"

#define INITDEPTH -3.0
#define FOV 450

#define MAXPLANE 6
#define MAXTRANSFORM (MAXPLANE+2)
#define EDIT_VIEW (MAXTRANSFORM-1)
#define EDIT_OBJECT (MAXTRANSFORM-2)
#define MAXMODEL 3

int tracking();
int shifted();
void limitrot();
void reshapewindow();
void drawscene();
void initgraphics();
void initparams();
void drawflipobj(flipobj *obj);
void drawcube(long outside, float s);
void drawcylinder(long outside, float s);
void drawsquare();
void fillsquare();
void setmatrix(long i);
void setinvmat(long i);
void farclip(long b);
void drawobj(void (*func)(), long mat);
void makemenus();
void changetitle();
float *quadmalloc(long quads);

void drawfobj();

void bigcube();
void middlecube();
void littlecube();

void bigcylinder();
void middlecylinder();
void littlecylinder();

void longcylinder();
void topcylinder();
void centercylinder();
void bottomcylinder();

long xdim,ydim;
float aspect;

float xr[MAXTRANSFORM] = {0,0,0,0,0,0,0,0};
float yr[MAXTRANSFORM] = {0,0,0,0,0,0,0,0};
float depth[MAXTRANSFORM] = {0,0,0,0,0,0,0,INITDEPTH};

float boink_xr;
float boink_yr;
float boink_depth;

long penable[MAXPLANE] = {0,0,0,0,0,0};
long pface[MAXPLANE] = {0,0,0,0,0,0};

float boink_rot_scale;
float boink_depth_scale;
short boink_screen_x;
short boink_screen_y;

int model;
int docap;
int motion;
int tselect;
int mselect;
int twoside;
int orangecaps;
int stenplanes;
int blocked = FALSE;
int ctrlkey = FALSE;
int leftshiftkey = FALSE;
int rightshiftkey = FALSE;
int leftmouse = FALSE;
int middlemouse = FALSE;
int menu,movemenu,disablemenu;
flipobj *obj = 0;

main(int argc, char *argv[]) {
    int i;
    short dev, val, screen_x, screen_y;

    /* check for correct machine support */
    if (getgdesc(GD_CLIPPLANES) == 0) {
	fprintf(stderr,"no user-defined clipping planes available.  abort.\n");
	exit(1);
    }

    stenplanes = getgdesc(GD_BITS_STENCIL);

    /* read model */
    if (argc > 1) {
	obj = readflipobj(argv[1]);
    }

    /* initialize the graphics */
    winopen("cap");
    initparams();
    initgraphics();

    qdevice(ESCKEY);
    qdevice(REDRAW);
    qdevice(CTRLKEY);
    qdevice(LEFTSHIFTKEY);
    qdevice(RIGHTSHIFTKEY);
    qdevice(CKEY);
    qdevice(DKEY);
    qdevice(LKEY);
    qdevice(MKEY);
    qdevice(OKEY);
    qdevice(RKEY);
    qdevice(VKEY);
    qdevice(ZKEY);
    qdevice(ZEROKEY);
    qdevice(ONEKEY);
    qdevice(TWOKEY);
    qdevice(THREEKEY);
    qdevice(FOURKEY);
    qdevice(FIVEKEY);
    qdevice(SPACEKEY);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    qdevice(RIGHTMOUSE);
    tie(LEFTMOUSE,MOUSEX,MOUSEY);
    tie(MIDDLEMOUSE,MOUSEX,MOUSEY);
    tie(RIGHTMOUSE,MOUSEX,MOUSEY);

    qenter(REDRAW,1);
    while(TRUE) {
	while (qtest() || blocked) {
	    dev = qread(&val);
		switch(dev) {
		    case ESCKEY:
			if (!val)
			    exit(0);
			break;
		    case REDRAW:
			reshapewindow();
			blocked = FALSE;
			break;
		    case LEFTSHIFTKEY:
			leftshiftkey = val;
			break;
		    case RIGHTSHIFTKEY:
			rightshiftkey = val;
			break;
		    case CTRLKEY:
			ctrlkey = val;
			break;
		    case ZEROKEY:
			if (val) {
			    if (shifted())
				penable[0] = FALSE;
			    else if (!tracking()) {
				penable[0] = TRUE;
				tselect = 0;
			    }
			    blocked = FALSE;
			}
			break;
		    case ONEKEY:
			if (val) {
			    if (shifted())
				penable[1] = FALSE;
			    else if (!tracking()) {
				penable[1] = TRUE;
				tselect = 1;
			    }
			    blocked = FALSE;
			}
			break;
		    case TWOKEY:
			if (val) {
			    if (shifted())
				penable[2] = FALSE;
			    else if (!tracking()) {
				penable[2] = TRUE;
				tselect = 2;
			    }
			    blocked = FALSE;
			}
			break;
		    case THREEKEY:
			if (val) {
			    if (shifted())
				penable[3] = FALSE;
			    else if (!tracking()) {
				penable[3] = TRUE;
				tselect = 3;
			    }
			    blocked = FALSE;
			}
			break;
		    case FOURKEY:
			if (val) {
			    if (shifted())
				penable[4] = FALSE;
			    else if (!tracking()) {
				penable[4] = TRUE;
				tselect = 4;
			    }
			    blocked = FALSE;
			}
			break;
		    case FIVEKEY:
			if (val) {
			    if (shifted())
				penable[5] = FALSE;
			    else if (!tracking()) {
				penable[5] = TRUE;
				tselect = 5;
			    }
			    blocked = FALSE;
			}
			break;
		    case CKEY:
			if (val) {
			    docap = !docap;
			    if (docap && !stenplanes){
			        docap = !docap; /* put it back the way it was */
				fprintf(stderr, "\nStencil planes do not exist on this machine.\nCapping will not work\.n");
			    }
			    blocked = FALSE;
			}
			break;
		    case DKEY:
			if (val) {
			    for (i=0; i<=MAXTRANSFORM; i++)
				printf("xr=%g yr=%g depth=%g\n",
				    xr[i],yr[i],depth[i]);
			}
			break;
		    case LKEY:
			if (val) {
			    twoside = !twoside;
			    if (twoside)
				lmbind(LMODEL,2);
			    else
				lmbind(LMODEL,1);
			    blocked = FALSE;
			}
			break;
		    case MKEY:
			if (val) {
			    motion = !motion;
			    mselect = tselect;
			    blocked = FALSE;
			}
			break;
		    case OKEY:
			if (val && !tracking()) {
			    tselect = EDIT_OBJECT;
			    blocked = FALSE;
			    }
			break;
		    case RKEY:
			if (val) {
			    initparams();
			    blocked = FALSE;
			}
			break;
		    case VKEY:
			if (val && !tracking()) {
			    tselect = EDIT_VIEW;
			    blocked = FALSE;
			    }
			break;
		    case ZKEY:
			if (val) {
			    orangecaps = !orangecaps;
			    blocked = FALSE;
			    }
			break;
		    case SPACEKEY:
			if (val) {
			    model += 1;
			    if (model >= MAXMODEL)
				model = 0;
			    blocked = FALSE;
			}
			break;
		    case LEFTMOUSE:
			if (shifted() && ctrlkey && val) {
			    for (i=0; i<MAXTRANSFORM; i++) {
				depth[i] = (i==EDIT_VIEW) ? INITDEPTH : 0;
			    }
			    tselect = EDIT_VIEW;
			    blocked = FALSE;
			}
			else if (val) {
			    leftmouse = TRUE;
			    dev = qread(&boink_screen_x);
			    dev = qread(&boink_screen_y);
			    boink_depth = depth[tselect];
			    boink_depth_scale = 0.002;
			    if (shifted())
				boink_depth_scale /= 10.0;
			    else if (ctrlkey)
				boink_depth_scale /= 100.0;
			    blocked = FALSE;
			}
			else if (leftmouse) {
			    leftmouse = FALSE;
			    dev = qread(&screen_x);
			    dev = qread(&screen_y);
			    depth[tselect] = boink_depth - boink_depth_scale *
				(float)(boink_screen_x - screen_x);
			    blocked = FALSE;
			}
			break;
		    case MIDDLEMOUSE:
			if (shifted() && ctrlkey && val) {
			    for (i=0; i<MAXTRANSFORM; i++) {
				xr[i] = 0.0;
				yr[i] = 0.0;
			    }
			    tselect = EDIT_VIEW;
			    blocked = FALSE;
			}
			else if (val) {
			    middlemouse = TRUE;
			    dev = qread(&boink_screen_x);
			    dev = qread(&boink_screen_y);
			    boink_yr = yr[tselect];
			    boink_xr = xr[tselect];
			    boink_rot_scale = 0.333;
			    if (shifted())
				boink_rot_scale /= 10.0;
			    else if (ctrlkey)
				boink_rot_scale /= 100.0;
			    blocked = FALSE;
			}
			else if (middlemouse) {
			    middlemouse = FALSE;
			    dev = qread(&screen_x);
			    dev = qread(&screen_y);
			    yr[tselect] = boink_yr + boink_rot_scale * 
				(float)(boink_screen_x - screen_x);
			    xr[tselect] = boink_xr + boink_rot_scale *
				(float)(boink_screen_y - screen_y);
			    limitrot();
			    blocked = FALSE;
			}
			break;
		    case RIGHTMOUSE:
			makemenus();
			if (val && !tracking()) {
			    switch(dopup(menu)) {
				case 100:
				    penable[0] = TRUE;
				    tselect = 0;
				    break;
				case 101:
				    penable[1] = TRUE;
				    tselect = 1;
				    break;
				case 102:
				    penable[2] = TRUE;
				    tselect = 2;
				    break;
				case 103:
				    penable[3] = TRUE;
				    tselect = 3;
				    break;
				case 104:
				    penable[4] = TRUE;
				    tselect = 4;
				    break;
				case 105:
				    penable[5] = TRUE;
				    tselect = 5;
				    break;
				case 110:
				    penable[0] = FALSE;
				    break;
				case 111:
				    penable[1] = FALSE;
				    break;
				case 112:
				    penable[2] = FALSE;
				    break;
				case 113:
				    penable[3] = FALSE;
				    break;
				case 114:
				    penable[4] = FALSE;
				    break;
				case 115:
				    penable[5] = FALSE;
				    break;
				case 116:
				    for (i=0; i<MAXPLANE; i++)
					penable[i] = FALSE;
				    break;
				case 200:
				    tselect = EDIT_VIEW;
				    break;
				case 201:
				    tselect = EDIT_OBJECT;
				    break;
				case 202:
				    motion = !motion;
				    mselect = tselect;
				    break;
				case 203:
				    docap = !docap;
			            if (docap && !stenplanes){
				       docap = !docap;
				       fprintf(stderr, "\nStencil planes do not exist on this machine.\nCapping will not work\.n");
				    }
				    break;
				case 204:
				    twoside = !twoside;
				    if (twoside)
					lmbind(LMODEL,2);
				    else
					lmbind(LMODEL,1);
				    break;
				case 205:
				    model += 1;
				    if (model >= MAXMODEL)
					model = 0;
				    break;
				case 206:
				    orangecaps = !orangecaps;
				    break;
				case 207:
				    initparams();
				    break;
				case 300:
				    exit(0);
				    break;
			    }
			    blocked = FALSE;
			}
			break;
		}
	    }
	if (leftmouse) {
	    depth[tselect] = boink_depth - boink_depth_scale *
		(float)(boink_screen_x - getvaluator(MOUSEX));
	}
	if (middlemouse) {
	    yr[tselect] = boink_yr + boink_rot_scale * 
		(float)(boink_screen_x - getvaluator(MOUSEX));
	    xr[tselect] = boink_xr + boink_rot_scale *
		(float)(boink_screen_y - getvaluator(MOUSEY));
	    limitrot();
	}
	if (motion && (!middlemouse || (mselect != tselect))) {
	    yr[mselect] += (getvaluator(MOUSEX)-640)/50.0;
	    xr[mselect] -= (getvaluator(MOUSEY)-512)/50.0;
	    limitrot();
	}
	drawscene();
	blocked = !tracking() && !motion;
    }
}

int tracking() {
    return leftmouse || middlemouse;
}

int shifted() {
    return leftshiftkey || rightshiftkey;
}

void limitrot() {
    while (xr[tselect] > 360) xr[tselect] -= 360;
    while (xr[tselect] < 0) xr[tselect] += 360;
    while (yr[tselect] > 360) yr[tselect] -= 360;
    while (yr[tselect] < 0) yr[tselect] += 360;
    if (motion && (mselect != tselect)) {
	while (xr[mselect] > 360) xr[mselect] -= 360;
	while (xr[mselect] < 0) xr[mselect] += 360;
	while (yr[mselect] > 360) yr[mselect] -= 360;
	while (yr[mselect] < 0) yr[mselect] += 360;
    }
}

void reshapewindow() {
    reshapeviewport();
    getsize(&xdim,&ydim);
    aspect = (float)xdim / (float)ydim;
    mmode(MVIEWING); 
    farclip(TRUE);		/* set perspective projection */
}

void farclip(long b) {
    if (b)
	perspective(FOV,aspect,0.1,5.0);
    else
	perspective(FOV,aspect,0.1,10000.0);
}

void setmatrix(long i) {
    /* do viewing transformation */
    translate(0.0,0.0,depth[EDIT_VIEW]);
    rot(-yr[EDIT_VIEW],'y');
    rot(xr[EDIT_VIEW],'x');
    /* do model transformation */
    rot(-yr[i],'y');
    rot(xr[i],'x');
    translate(0.0,0.0,depth[i]);
}

void setinvmat(long i) {
    /* compute inverse of model transformation */
    translate(0.0,0.0,-depth[i]);
    rot(-xr[i],'x');
    rot(yr[i],'y');
    /* compute inverse of viewing transformation */
    rot(-xr[EDIT_VIEW],'x');
    rot(yr[EDIT_VIEW],'y');
    translate(0.0,0.0,-depth[EDIT_VIEW]);
}

float cp[4] = {0.0, 0.0, 1.0, 0.0};

void drawscene() {
    register i;

    /* update title bar */
    changetitle();

    /* clear the screen */
    czclear(0,0);
    sclear(0);

    /* modify the location of the current clipplane */
    for (i=0; i<MAXPLANE; i++) {
	if ((tselect == i) || (tselect == EDIT_VIEW) ||
		(motion && (mselect == i))) {
	    /* determine which way the clipping plane faces */
	    float mat[16];
	    pushmatrix();
	    setinvmat(i);
	    getmatrix(mat);
	    pface[i] = (mat[14] < 0) ? 1 : 0;
	    popmatrix();
	    /* relocate the clipping plane */
	    pushmatrix();
	    setmatrix(i);
	    clipplane(i,CP_DEFINE,cp);
	    popmatrix();
	}
    }

    /* disable all clipping planes */
    for (i=0; i<MAXPLANE; i++) {
	clipplane(i,CP_OFF,0);
    }

    /* draw the current clipping plane */
    if ((tselect < MAXPLANE) && penable[tselect]) {
	if (pface[tselect])
	    cpack(0xffffffff);
	else
	    cpack(0xff00ff00);
	pushmatrix();
	setmatrix(tselect);
	drawsquare();
	popmatrix();
    }

    switch (model) {
    case 0:
	if (obj) {
	    drawobj(drawfobj,1);
	} else {
	    drawobj(bigcube,1);
	    drawobj(middlecube,3);
	    drawobj(littlecube,5);
	}
	break;
    case 1:
	drawobj(bigcylinder,1);
	drawobj(middlecylinder,3);
	drawobj(littlecylinder,5);
	break;
    case 2:
	drawobj(longcylinder,1);
	drawobj(topcylinder,3);
	drawobj(centercylinder,4);
	drawobj(bottomcylinder,5);
	break;
    }

    swapbuffers();
};

void drawobj(void (*func)(), long mat) {
    register i;

    /* setup the matrix for drawing the object */
    pushmatrix(); 
    setmatrix(EDIT_OBJECT);

    /* enable selected clipping planes - draw object */
    for (i=0; i<MAXPLANE; i++) {
	if (penable[i])
	    clipplane(i,CP_ON,0);
	else
	    clipplane(i,CP_OFF,0);
    }
    if (docap)
	backface(TRUE);
    lmbind(MATERIAL,mat);
    (*func)();
    lmbind(MATERIAL,0);
    if (docap)
	backface(FALSE);

    /* done if capping is not enabled */
    if (!docap) {
	popmatrix();
	return;
    }

    /* enable only front clipping planes - draw into stencil bitplanes */
    for (i=0; i<MAXPLANE; i++) {
	if (penable[i] && pface[i])
	    clipplane(i,CP_ON,0);
	else
	    clipplane(i,CP_OFF,0);
    }
    farclip(FALSE);
    zbuffer(FALSE);
    wmpack(0x00000000);
    if (stenplanes) stencil(TRUE,0,SF_ALWAYS,0x1,ST_INVERT,ST_INVERT,ST_INVERT);
    (*func)();
    farclip(TRUE);
    zbuffer(TRUE);
    wmpack(0xffffffff);
    popmatrix();

    /* draw caps */
    for (i=0; i<MAXPLANE; i++) {
	if (penable[i])
	    clipplane(i,CP_ON,0);
	else
	    clipplane(i,CP_OFF,0);
    }
    if (stenplanes) stencil(TRUE,1,SF_EQUAL,0x1,ST_KEEP,ST_KEEP,ST_ZERO);
    zfunction(ZF_ALWAYS);
    for (i=0; i<MAXPLANE; i++) {
	if (penable[i] && pface[i]) {
	    clipplane(i,CP_OFF,0);
	    pushmatrix();
	    setmatrix(i);
	    if (orangecaps)
		cpack(0x007fff);
	    else
		lmbind(MATERIAL,mat);
	    fillsquare();
	    lmbind(MATERIAL,0);
	    popmatrix();
	    clipplane(i,CP_ON,0);
	}
    }
    zfunction(ZF_GEQUAL);
    if (stenplanes) stencil(FALSE,0,0,0,0,0,0);
}

void drawfobj() {
    drawflipobj(obj);
}

void bigcube() {
    drawcube(1, 0.5);
    drawcube(0, 0.45);
}

void middlecube() {
    drawcube(1, 0.3);
    drawcube(0, 0.25);
}

void littlecube() {
    drawcube(1, 0.1);
    drawcube(0, 0.05);
}

void bigcylinder() {
    drawcylinder(1, 0.5);
    drawcylinder(0, 0.45);
}

void middlecylinder() {
    drawcylinder(1, 0.3);
    drawcylinder(0, 0.25);
}

void littlecylinder() {
    drawcylinder(1, 0.1);
    drawcylinder(0, 0.05);
}

void longcylinder() {
    drawcylinder(1, 0.5);
}

void topcylinder() {
    pushmatrix();
    translate(0.0, 0.0, 0.4);
    rot(90.0,'x');
    drawcylinder(1, 0.3);
    popmatrix();
}

void centercylinder() {
    pushmatrix();
    rot(90.0,'x');
    rot(45.0,'y');
    drawcylinder(1, 0.3);
    popmatrix();
}

void bottomcylinder() {
    pushmatrix();
    translate(0.0, 0.0, -0.4);
    rot(90.0,'x');
    rot(90.0,'y');
    drawcylinder(1, 0.3);
    popmatrix();
}

float cubevertex[8][4] = {
    {-1.0, -1.0, -1.0, 1.0},
    { 1.0, -1.0, -1.0, 1.0},
    {-1.0,  1.0, -1.0, 1.0},
    { 1.0,  1.0, -1.0, 1.0},
    {-1.0, -1.0,  1.0, 1.0},
    { 1.0, -1.0,  1.0, 1.0},
    {-1.0,  1.0,  1.0, 1.0},
    { 1.0,  1.0,  1.0, 1.0},
};

float cubenormal[6][4] = {
    { 0.0,  0.0, -1.0},
    { 1.0,  0.0,  0.0},
    { 0.0,  0.0,  1.0},
    {-1.0,  0.0,  0.0},
    { 0.0,  1.0,  0.0},
    { 0.0, -1.0,  0.0},
};

void drawcube(long outside, float s) {
    pushmatrix();
    scale(s,s,s);
    if (outside) {
	bgnpolygon();
	n3f(cubenormal[0]);
	v3f(cubevertex[2]);
	v3f(cubevertex[3]);
	v3f(cubevertex[1]);
	v3f(cubevertex[0]);
	endpolygon();
	bgnpolygon();
	n3f(cubenormal[1]);
	v3f(cubevertex[3]);
	v3f(cubevertex[7]);
	v3f(cubevertex[5]);
	v3f(cubevertex[1]);
	endpolygon();
	bgnpolygon();
	n3f(cubenormal[2]);
	v3f(cubevertex[7]);
	v3f(cubevertex[6]);
	v3f(cubevertex[4]);
	v3f(cubevertex[5]);
	endpolygon();
	bgnpolygon();
	n3f(cubenormal[3]);
	v3f(cubevertex[6]);
	v3f(cubevertex[2]);
	v3f(cubevertex[0]);
	v3f(cubevertex[4]);
	endpolygon();
	bgnpolygon();
	n3f(cubenormal[4]);
	v3f(cubevertex[6]);
	v3f(cubevertex[7]);
	v3f(cubevertex[3]);
	v3f(cubevertex[2]);
	endpolygon();
	bgnpolygon();
	n3f(cubenormal[5]);
	v3f(cubevertex[0]);
	v3f(cubevertex[1]);
	v3f(cubevertex[5]);
	v3f(cubevertex[4]);
	endpolygon();
    }
    else {
	bgnpolygon();
	n3f(cubenormal[2]);
	v3f(cubevertex[0]);
	v3f(cubevertex[1]);
	v3f(cubevertex[3]);
	v3f(cubevertex[2]);
	endpolygon();
	bgnpolygon();
	n3f(cubenormal[3]);
	v3f(cubevertex[1]);
	v3f(cubevertex[5]);
	v3f(cubevertex[7]);
	v3f(cubevertex[3]);
	endpolygon();
	bgnpolygon();
	n3f(cubenormal[0]);
	v3f(cubevertex[5]);
	v3f(cubevertex[4]);
	v3f(cubevertex[6]);
	v3f(cubevertex[7]);
	endpolygon();
	bgnpolygon();
	n3f(cubenormal[1]);
	v3f(cubevertex[4]);
	v3f(cubevertex[0]);
	v3f(cubevertex[2]);
	v3f(cubevertex[6]);
	endpolygon();
	bgnpolygon();
	n3f(cubenormal[5]);
	v3f(cubevertex[2]);
	v3f(cubevertex[3]);
	v3f(cubevertex[7]);
	v3f(cubevertex[6]);
	endpolygon();
	bgnpolygon();
	n3f(cubenormal[4]);
	v3f(cubevertex[4]);
	v3f(cubevertex[5]);
	v3f(cubevertex[1]);
	v3f(cubevertex[0]);
	endpolygon();
    }
    popmatrix();
}

#define SLICES 80
#define LENGTH 1
#define WIDTH 0.5
#define PI (3.141592654)

void drawcylinder(long outside, float s) {
    static init = 0;
    register i;
    float angle;
    static float *vtop,*vbot,*noutside,*ninside;
    static float ntop[3] = {0,0,1};
    static float nbot[3] = {0,0,-1};
    static float vtopcenter[3] = {0,0,LENGTH};
    static float vbotcenter[3] = {0,0,-LENGTH};

    if (!init) {
	init = 1;
	vtop = quadmalloc(SLICES+1);
	vbot = quadmalloc(SLICES+1);
	noutside = quadmalloc(SLICES+1);
	ninside = quadmalloc(SLICES+1);
	for (i=0; i<=SLICES; i++) {
	    angle = ((float)i / (float)SLICES) * PI * 2.0;
	    vtop[4*i+0] = vbot[4*i+0] = fcos(angle) * WIDTH;
	    vtop[4*i+1] = vbot[4*i+1] = fsin(angle) * WIDTH;
	    vtop[4*i+2] = LENGTH;
	    vbot[4*i+2] = -LENGTH;
	    noutside[4*i+0] = fcos(angle);
	    noutside[4*i+1] = fsin(angle);
	    ninside[4*i+0] = -fcos(angle);
	    ninside[4*i+1] = -fsin(angle);
	    noutside[4*i+2] = 0;
	    ninside[4*i+2] = 0;
	}
    }

    pushmatrix();
    scale(s,s,s);

    /* draw top of cylinder */
    bgntmesh();
    n3f(outside ? ntop : nbot);
    v3f(vtopcenter);
    for (i=0; i<=SLICES; i++) {
	v3f(&vtop[outside ? (4*i) : (4*(SLICES-i))]);
	swaptmesh();
    }
    endtmesh();

    /* draw bottom of cylinder */
    bgntmesh();
    n3f(outside ? nbot : ntop);
    v3f(vbotcenter);
    for (i=0; i<=SLICES; i++) {
	v3f(&vbot[outside ? (4*(SLICES-i)) : (4*i)]);
	swaptmesh();
    }
    endtmesh();

    /* draw sides of cylinder */
    for (i=0; i<SLICES; i++) {
	bgnpolygon();
	if (outside) {
	    n3f(&noutside[4*i]);
	    v3f(&vtop[4*i]);
	    v3f(&vbot[4*i]);
	    n3f(&noutside[4*(i+1)]);
	    v3f(&vbot[4*(i+1)]);
	    v3f(&vtop[4*(i+1)]);
	}
	else {
	    n3f(&ninside[4*i]);
	    v3f(&vbot[4*i]);
	    v3f(&vtop[4*i]);
	    n3f(&ninside[4*(i+1)]);
	    v3f(&vtop[4*(i+1)]);
	    v3f(&vbot[4*(i+1)]);
	}
	endpolygon();
    }

    popmatrix();
}

float squarevertex[4][4] = {
    {-1.0, -1.0, 0.0, 1.0},
    { 1.0, -1.0, 0.0, 1.0},
    {-1.0,  1.0, 0.0, 1.0},
    { 1.0,  1.0, 0.0, 1.0},
};

float squarenormal[4] = {0.0, 0.0, -1.0, 0.0};

void drawsquare() {
    bgnclosedline();
    v3f(squarevertex[0]);
    v3f(squarevertex[1]);
    v3f(squarevertex[3]);
    v3f(squarevertex[2]);
    endclosedline();
}

void fillsquare() {
    pushmatrix();
    scale(100.0,100.0,100.0);
    bgnpolygon();
    n3f(squarenormal);
    v3f(squarevertex[0]);
    v3f(squarevertex[2]);
    v3f(squarevertex[3]);
    v3f(squarevertex[1]);
    endpolygon();
    popmatrix();
}

float *quadmalloc(long quads) {
    float *f;
    f = (float*)malloc((4*quads+3)*4);
    return (float*)(((long)f + 0xc) & 0xfffffff0);
}

void makemenus() {
    char s[1000];

    sprintf(s,"Options%%t|move view (v)%%x200|move object (o)%%x201|move/enable plane%%m|disable plane%%m|%s motion (m) %%x202|%s capping (c)%%x203|%s lighting (l)%%x204|change model (space) %%x205|%s caps (z) %%x206|reset (r)%%x207|quit (esc)%%x300",motion?"disable":"enable",docap?"disable":"enable",twoside?"oneside":"twoside",orangecaps?"lighted":"orange");

    movemenu = defpup("0%x100|1%x101|2%x102|3%x103|4%x104|5%x105");
    disablemenu = defpup("0%x110|1%x111|2%x112|3%x113|4%x114|5%x115|all%x116");
    menu = defpup(s,movemenu,disablemenu);
}

void changetitle() {
    static char oldtitle[100] = "";
    char title[100];
    if (tselect < MAXPLANE)
	sprintf(title,"cap: move plane %d",tselect);
    else if (tselect == EDIT_OBJECT)
	sprintf(title,"cap: move object");
    else if (tselect == EDIT_VIEW)
	sprintf(title,"cap: move view");
    else
	sprintf(title,"cap:");
    if (strcmp(title,oldtitle)) {
	strcpy(oldtitle,title);
	wintitle(title);
    }
}

float brass[] = {
    AMBIENT, 0.35, 0.25,  0.1,
    DIFFUSE, 0.65, 0.5, 0.35,
    SPECULAR, 0.0, 0.0, 0.0,
    SHININESS, 5.0,
    LMNULL
};

float pewter[] = {
    AMBIENT, 0.0, 0.0, 0.0,
    DIFFUSE, 0.6, 0.55, 0.65,
    SPECULAR, 0.9, 0.9, 0.95,
    SHININESS, 10.0,
    LMNULL
};

float redplastic[] = {
    AMBIENT, 0.3, 0.1, 0.1,
    DIFFUSE, 0.5, 0.1, 0.1,
    SPECULAR, 0.45, 0.45, 0.45,
    SHININESS, 30.0,
    LMNULL
};

float greenplastic[] = {
    AMBIENT, 0.1, 0.3, 0.1,
    DIFFUSE, 0.1, 0.5, 0.1,
    SPECULAR, 0.45, 0.45, 0.45,
    SHININESS, 30.0,
    LMNULL
};

float blueplastic[] = {
    AMBIENT, 0.1, 0.1, 0.3,
    DIFFUSE, 0.1, 0.1, 0.5,
    SPECULAR, 0.45, 0.45, 0.45,
    SHININESS, 30.0,
    LMNULL
};

float whitelight[] = {
    AMBIENT, 0.0, 0.0, 0.0, 
    LCOLOR, 1.0, 1.0, 1.0, 
    POSITION, 0.0, 0.0, 1.0, 0.0,
    LMNULL
};
		    
float infinite[] = {
    AMBIENT, 0.3, 0.3, 0.3, 
    LOCALVIEWER, 0.0, 
    LMNULL
};
		    
float infinite2[] = {
    AMBIENT, 0.3, 0.3, 0.3, 
    LOCALVIEWER, 0.0, 
    TWOSIDE, 1.0,
    LMNULL
};

void initparams() {
    register i;
    for (i=0; i<MAXPLANE; i++)
	penable[i] = FALSE;
    for (i=0; i<MAXTRANSFORM; i++) {
	xr[i] = 0.0;
	yr[i] = 0.0;
	depth[i] = 0.0;
    }
    depth[EDIT_VIEW] = INITDEPTH;
    tselect = EDIT_VIEW;
    mselect = EDIT_VIEW;
    motion = FALSE;
    docap = FALSE;
    twoside = TRUE;
    model = 0;
    orangecaps = 0;
}

void initgraphics() {
    RGBmode();
    doublebuffer();
    if(stenplanes) stensize(1);
    gconfig();
    subpixel(TRUE);
    reshapewindow();
    zbuffer(TRUE);
    zfunction(ZF_GEQUAL);
    lsetdepth(0x7fffff,0);
    lmdef(DEFMATERIAL, 1, 0, brass);
    lmdef(DEFMATERIAL, 2, 0, pewter);
    lmdef(DEFMATERIAL, 3, 0, redplastic);
    lmdef(DEFMATERIAL, 4, 0, greenplastic);
    lmdef(DEFMATERIAL, 5, 0, blueplastic);
    lmdef(DEFLIGHT,    1, 0, whitelight);
    lmdef(DEFLMODEL,   1, 0, infinite);
    lmdef(DEFLMODEL,   2, 0, infinite2);
    lmbind(LIGHT1, 1);
    if (twoside)
	lmbind(LMODEL, 2);
    else
	lmbind(LMODEL, 1);
}



flipobj
*readflipobj(name)
char *name;
{
	FILE	*inf;
	flipobj	*obj;
	int		i, j;
	int		nlongs;
	int		magic;
	int		*ip;
	char s[200];

	inf = fopen(name,"r");
	if(!inf) {
		sprintf(s,"%s%s",MODELDIR,name);
		inf = fopen(s,"r");
	}
	if(!inf) {
		fprintf(stderr,"readfast: can't open input file %s\n",name);
		fprintf(stderr,"          can't open input file %s\n",s);
		exit(1);
	}
	fread(&magic,sizeof(int),1,inf);
	if(magic != FASTMAGIC) {
	fprintf(stderr,"readfast: bad magic in object file\n");
	fclose(inf);
		exit(1);
	}
	obj = (flipobj *)malloc(sizeof(flipobj));
	fread(&obj->npoints,sizeof(int),1,inf);
	/*** IGNORE COLORS FIELD ***/
	fread(&magic,sizeof(int),1,inf);

	/*
	 * Insure that the data is quad-word aligned and begins on a page
	 * boundary.  This shields us from the performance loss which occurs 
	 * whenever we try to fetch data which straddles a page boundary (the OS
	 * has to map in the next virtual page and re-start the DMA transfer).
	 */
	nlongs = 8 * obj->npoints;
	obj->data = (float *) malloc(nlongs*sizeof(int) + 4096);
	obj->data = (float *) (((int)(obj->data)) + 0xfff);
	obj->data = (float *) (((int)(obj->data)) & 0xfffff000);

	for (i = 0, ip = (int *)obj->data;  i < nlongs/4;  i++, ip += 4)
		fread(ip, 3 * sizeof(int), 1, inf);
	fclose(inf);

	return obj;
}

void
drawflipobj(obj)
flipobj *obj;
{
	register float *p,*end;

	p = obj->data;
	end = p + 8 * obj->npoints;

	while ( p < end) {
		bgnpolygon();
		n3f(p);
		v3f(p+4);
		n3f(p+8);
		v3f(p+12);
		n3f(p+16);
		v3f(p+20);
		n3f(p+24);
		v3f(p+28);
		endpolygon();
		p += 32;
	}
}


/*
 * objmaxpoint
 *
 * find the vertex farthest from the origin,
 * so we can set the near and far clipping planes tightly.
 */

#define MAXVERT(v) if ( (len = sqrt(	(*(v))  *  (*(v))  +	  \
					(*(v+1)) * (*(v+1)) +		   \
					(*(v+2)) * (*(v+2)) )) > max)  \
			max = len;

float
objmaxpoint(obj)
flipobj *obj;
{
	register float *p, *end;
	register int npolys;
	register float len;
	register float max = 0.0;

	p = obj->data;

	end = p + 8 * obj->npoints;
	while ( p < end) {
		MAXVERT(p+4);
		MAXVERT(p+12);
		MAXVERT(p+20);
		MAXVERT(p+28);
		p += 32;
	}

	return max;
}



