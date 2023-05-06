/*
 *	miscgl - 
 *		Miscellaneous stuff that relies on the gl.
 *
 *			    Paul Haeberli - 1988
 */
#include "values.h"
#include "math.h"
#include "stdio.h"
#include "gl.h"
#include "device.h"

static makecirc();


drawfunc(f)
int (*f)();
{
    short val;
    static int first_time = 1;

    if(first_time == 1) {
	first_time = 0;
	qdevice(LEFTARROWKEY);
	qdevice(RIGHTARROWKEY);
	qdevice(UPARROWKEY);
	qdevice(DOWNARROWKEY);
    }
    f();
    while (1) {
	switch(qread(&val)) {
	    case LEFTARROWKEY:	if(val) deltawin(-1, 0); break;
	    case RIGHTARROWKEY:	if(val) deltawin( 1, 0); break;
	    case UPARROWKEY:	if(val) deltawin( 0, 1); break;
	    case DOWNARROWKEY:	if(val) deltawin( 0,-1); break;

	    case REDRAW:
		qreset();
		f();
		break;

	    case ESCKEY:
		if (!val) {
		    gexit();
		    exit(0);
		}
		break;
	}
    }
}

deltawin(x,y)
int x,y;
{
    long xo,yo;

    getorigin(&xo,&yo);
    xo += x;
    yo += y;
    winmove(xo,yo);
}

qmouse()
{
    qdevice(MOUSEX);
    qdevice(MOUSEY);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    qdevice(RIGHTMOUSE);
}

getmousex()
{
    long xorg, yorg;

    getorigin(&xorg,&yorg);
    return getvaluator(MOUSEX)-xorg;
}

getmousey()
{
    long xorg, yorg;

    getorigin(&xorg,&yorg);
    return getvaluator(MOUSEY)-yorg;
}

float fgetmousex()
{
    long xorg, yorg;
    long xsize, ysize;

    getsize(&xsize,&ysize);
    getorigin(&xorg,&yorg);
    return ((float)getvaluator(MOUSEX)-xorg)/(float)xsize;
}

float fgetmousey()
{
    long xorg, yorg;
    long xsize, ysize;

    getsize(&xsize,&ysize);
    getorigin(&xorg,&yorg);
    return ((float)getvaluator(MOUSEY)-yorg)/(float)ysize;
}

printsize()
{
    long xsize, ysize;

    getsize(&xsize,&ysize);
    printf("size is %d %d\n",xsize,ysize);
}

printorigin()
{
    long xorg, yorg;

    getorigin(&xorg,&yorg);
    printf("size is %d %d\n",xorg,yorg);
}

/*
 *	redraw -
 *		Test for a redraw token in the queue, and if present, 
 * 	swallow it and call the optionally supplied user function as well 
 *	as the code to reshape the viewport based on the new size.
 *
 *				Kipp Hickman - 1985
 */
redraw(f)
int (*f)();
{
    int retval;
    short v;

    retval = 0;
    while (qtest()) {
	switch (qread(&v)) {
	    case REDRAW:
		reshapeviewport();
		if (f)
		    (*f)();
		retval = 1;
		break;
	}
    }
    return retval;
}

/*
 *	subview -
 *		Some support for viewports inside the graph port.
 *
 *				Henry Moreton - 1984
 *
 */
subviewport(left, right, bottom, top)
long left, right, bottom, top;
{
    int xmaxscreen, ymaxscreen;

    xmaxscreen = getgdesc(GD_XPMAX)-1;
    ymaxscreen = getgdesc(GD_YPMAX)-1;
    subport((float)left/xmaxscreen, (float)right/xmaxscreen, 
	     (float)bottom/ymaxscreen, (float)top/ymaxscreen);
}

subport(left, right, bottom, top)
float left, right, bottom, top;
{
    short curr_left, curr_right, curr_bottom, curr_top;
    short new_left, new_right, new_bottom, new_top;
    int width, height;

    getviewport(&curr_left,&curr_right,&curr_bottom,&curr_top);

    /* calculate the new viewport size and position based on the
    current viewport and the requested subviewport */

    height = (curr_top - curr_bottom + 1);
    width = (curr_right - curr_left + 1);
    new_right = curr_left + (right * width) + 0.5;
    new_left = curr_left + (left * width) + 0.5;
    new_top = curr_bottom + (top * height) + 0.5;
    new_bottom = curr_bottom + (bottom * height) + 0.5;
    viewport(new_left, new_right, new_bottom, new_top);
}

fillrect(x1,y1,x2,y2)
float x1,y1,x2,y2;
{
    float v[2];

    bgnpolygon();
    v[0] = x1;
    v[1] = y1;
    v2f(v);
    v[0] = x2;
    v2f(v);
    v[1] = y2;
    v2f(v);
    v[0] = x1;
    v2f(v);
    endpolygon();
}

drawrect(x1,y1,x2,y2)
float x1,y1,x2,y2;
{
    float v[2];

    bgnclosedline();
    v[0] = x1;
    v[1] = y1;
    v2f(v);
    v[0] = x2;
    v2f(v);
    v[1] = y2;
    v2f(v);
    v[0] = x1;
    v2f(v);
    endclosedline();
}

fillrecti(x1,y1,x2,y2)
int x1,y1,x2,y2;
{
    long v[2];

    bgnpolygon();
    v[0] = x1;
    v[1] = y1;
    v2i(v);
    v[0] = x2;
    v2i(v);
    v[1] = y2;
    v2i(v);
    v[0] = x1;
    v2i(v);
    endpolygon();
}

drawrecti(x1,y1,x2,y2)
int x1,y1,x2,y2;
{
    long v[2];

    bgnclosedline();
    v[0] = x1;
    v[1] = y1;
    v2i(v);
    v[0] = x2;
    v2i(v);
    v[1] = y2;
    v2i(v);
    v[0] = x1;
    v2i(v);
    endclosedline();
}

fillcirc(x,y,rad)
float x, y, rad;
{
    pushmatrix();
    translate(x,y,0.0);
    scale(rad,rad,0.0);
    bgnpolygon();
    makecirc();
    endpolygon();
    popmatrix();
}

drawcirc(x,y,rad)
float x, y, rad;
{
    pushmatrix();
    translate(x,y,0.0);
    scale(rad,rad,0.0);
    bgnclosedline();
    makecirc();
    endclosedline();
    popmatrix();
}

static nsides = 0;
static float *circcoords;

circsides(n)
int n;
{
    int i;
    float a;

    if(n != nsides) {
	nsides = n;
	if(circcoords) 
	    free(circcoords);
	circcoords = (float *)malloc(2*nsides*sizeof(float));
	for(i=0; i<nsides; i++) {
	    a = ((i)*2*M_PI)/nsides;
	    circcoords[2*i+0] = sin(a);
	    circcoords[2*i+1] = cos(a);
	}
    }
}

static makecirc()
{
    int i, n;
    float *fptr;

    if(!circcoords)
	circsides(40);
    fptr = circcoords;
    n = nsides;
    while(n>8) {
	v2f(fptr+0);
	v2f(fptr+2);
	v2f(fptr+4);
	v2f(fptr+6);
	v2f(fptr+8);
	v2f(fptr+10);
	v2f(fptr+12);
	v2f(fptr+14);
	fptr += 16;
	n-=8;
    }
    while(n--) {
	v2f(fptr);
	fptr += 2;
    }
}

shiftdown()
{
    if(getbutton(LEFTSHIFTKEY) || getbutton(RIGHTSHIFTKEY))
	return 1;
    else
	return 0;
}

optiondown()
{
    if(getbutton(LEFTALTKEY) || getbutton(RIGHTALTKEY))
	return 1;
    else
	return 0;
}

spacedown()
{
    if(getbutton(SPACEKEY))
	return 1;
    else
	return 0;
}
