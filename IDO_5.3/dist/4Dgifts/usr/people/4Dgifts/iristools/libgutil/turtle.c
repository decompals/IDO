/*
 *	turtle -
 * 		A simple implementation of turtle graphics.
 *
 *				Paul Haeberli - 1984
 *
 */
#include "values.h"
#include "math.h"
#include "gl.h"

#define RADIANS(a) ((a)*(M_PI/180.0))
#define INVISIBLE -1
#define NONE -1

static drawline();
static tg_addpolypoint();


static float lastx = 10000,
	     lasty = 10000,
	     lastz = 10000;

static float xposition = 0.0;
static float yposition = 0.0;
static float zposition = 0.0;
static float uposition = 0.0;
static float vposition = 0.0;
static float heading = 0.0;
static int  penc = INVISIBLE;
static int  pent = 0;
static int  fillc = INVISIBLE;
static int  fillt = 0;
static int  tgpolypoints = 0;
float tgpoly[1000][3];

/*
 *	CLEARSCREEN - erase the entire screen area.
 */
CLEARSCREEN()
{
    clear();
}

/*
 *	MOVE - move some distance.
 */
MOVE( distance )
float distance;
{
    float x, y;

    x = uposition + distance * cos( RADIANS( heading ) );
    y = vposition + distance * sin( RADIANS( heading ) );
    MOVETO( x, y );
}

/*
 *	MOVETO - move to a cartesian coordinate.
 */
MOVETO(x,y)
float x, y;
{
    if (penc != INVISIBLE)
	drawline(uposition,vposition,0.0,x,y,0.0);
    uposition = x;
    vposition = y;
    tg_addpolypoint(x,y,0.0); 
}

/*
 *	SPACETO - move to a point in 3 space.
 */
SPACETO( x, y, z )
float x, y, z;
{
    if (penc != INVISIBLE)
	drawline(xposition,yposition,zposition,x,y,z);
    xposition = x;
    yposition = y;
    zposition = z;
    tg_addpolypoint(x,y,z);
}

/*
 *	TURN - turn the turtle.
 */
TURN(angle)
float	angle;
{
    heading =  heading + angle;
    heading -= 360 * (int)( heading / 360.0 );
}

/*
 *	TURNTO - turn to a specific angle.
 */
TURNTO(angle)
float	angle;
{
    heading =  angle;
    heading -= 360 * (int)(heading / 360.0);
}

/*
 *	FILLCOLOR 
 *	FILLTEXTURE
 *	PENCOLOR
 *	PENTYPE - change the fillcolor, filltexture, pencolor, and pentype.
 */
FILLCOLOR( fillcolor )
int fillcolor;
{
    fillc = fillcolor;
}

FILLTEXTURE( filltype )
int filltype;
{
    fillt = filltype;
}

PENCOLOR( threadcolor )
int threadcolor;
{
    penc = threadcolor;
    if (penc != INVISIBLE)
	color(penc); 
}

PENTYPE(threadtype)
int threadtype;
{
    pent = threadtype;
}

/*
 *	WHATCOLOR - return the current color.	
 */
WHATCOLOR()
{
    return penc;
}

/*
 *	WHEREAMI - return our current heading and position.
 */
WHEREAMI(x,y,h)
float *x,*y;
float *h;
{
    *x = uposition;
    *y = vposition;
    *h = heading;
}

/*
 *	BEGINPOLY - start a polygon.
 */
BEGINPOLY()
{
    tgpolypoints = 0; 
}

/*
 *	ENDPOLY - close and fill a polygon.
 */
ENDPOLY()
{
    int i;

    bgnpolygon();
    for(i=0; i<tgpolypoints; i++) 
	v3f(tgpoly[i]);
    endpolygon();
}

/*
 *	TEXT - put some text.
 */
TEXT( x, y, string )
float x, y;
char *string;
{
    cmov2( x, y );
    charstr( string );
}

/*
 *	VIEWPORT - set the viewport.
 */
VIEWPORT(x1,y1,x2,y2)
float x1, x2, y1, y2;
{
    viewport(x1,y1,x2,y2);
}

/*
 *	WINDOW - set the window.
 */
WINDOW(x1,y1,x2,y2)
float x1, x2, y1, y2;
{
    ortho2(x1,y1,x2,y2); 
}

static drawline(x1,y1,z1,x2,y2,z2)
float x1, y1, z1, x2, y2, z2;
{
    if ((x1 != lastx) || (y1 != lasty) || (z1 != lastz))
        move(x1,y1,z1);
    draw(x2,y2,z2);
    lastx = x2;
    lasty = y2;
    lastz = z2;
}

static tg_addpolypoint(x,y,z)
float x, y, z;
{
    tgpoly[tgpolypoints][0] = x;
    tgpoly[tgpolypoints][1] = y;
    tgpoly[tgpolypoints][2] = z;
    tgpolypoints = (tgpolypoints+1) % 1000;
}
