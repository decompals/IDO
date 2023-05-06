/* 
 *	track -
 *		Simple track ball interface
 *	
 *			Originally by Gavin Bell
 *			Rehacked by Paul Haeberli
 *	exports:
 *		trackclick();
 *		trackpoll();
 *		tracktransform();
 *
 */
#include "gl.h"
#include "device.h"
#include "math.h"
#include "vect.h"

static tinit();

#define SQRT2		1.41421356237309504880
#define SQRT1_2		0.70710678118654752440

static vect trans;
static float rvec[4];
static float spinrot[4];
static int firsted;
static int omx, omy;

trackclick()
{
    omx = getvaluator(MOUSEX);
    omy = getvaluator(MOUSEY);
}

trackpoll()
{
    int mstate;
    int mx, my;
    int dx, dy, del;
    long xorg, yorg;
    long xsize, ysize;
    float x1, y1, x2, y2;
    float r[4];

    if(!firsted) {
	tinit();
	firsted = 1;
    }
    getorigin(&xorg,&yorg);
    getsize(&xsize,&ysize);
    mstate = 0;
    if(getbutton(MIDDLEMOUSE)) 
	mstate |= 1;
    if(getbutton(LEFTMOUSE)) 
	mstate |= 2;
    mx = getvaluator(MOUSEX);
    my = getvaluator(MOUSEY);
    dx = mx-omx;
    dy = my-omy;
    switch(mstate) {
	case 0:
	    break;
	case 1:
	    trans.x += (float)dx/xsize;
	    trans.y += (float)dy/xsize;
	    break;
	case 2:
	    x1 = (2.0*(float)(omx-xorg)/xsize)-1.0;
	    y1 = (2.0*(float)(omy-yorg)/ysize)-1.0;
	    x2 = (2.0*(float)( mx-xorg)/xsize)-1.0;
	    y2 = (2.0*(float)( my-yorg)/ysize)-1.0;
	    trackball(r,x1,y1,x2,y2);
    	    spinrot[0] = r[0];
    	    spinrot[1] = r[1];
    	    spinrot[2] = r[2];
    	    spinrot[3] = r[3];
	    break;
	case 3:
	    del = dx+dy;
	    trans.z += 4.0*(float)del/xsize;
	    break;
    }
    omx = mx;
    omy = my;
}

gettracktransform(mat)
float mat[4][4];
{
    if(!firsted) {
	tinit();
	firsted = 1;
    }
    add_quats(spinrot,rvec,rvec);
    pushmatrix();
    myidentity();
    translate(trans.x,trans.y,trans.z);
    build_rotmatrix(mat,rvec);
    multmatrix(mat);
    getmatrix(mat);
    popmatrix();
}

tracktransform()
{
    float m[4][4];

    gettracktransform(m);
    multmatrix(m);
}

static tinit()
{
    vset(&trans,0.0,0.0,0.0);
    rvec[0] = 0.00;
    rvec[1] = 0.00;
    rvec[2] = 0.00;
    rvec[3] = 1.00;
    trackball(spinrot,0.0,-0.1,0.01,-0.1);
}

/*
 *	Implementation of a virtual trackball.
 *	Implemented by Gavin Bell, lots of ideas from Thant Tessman and
 *    	the August '88 issue of Siggraph's "Computer Graphics," pp. 121-129.
 *
 */
#define RENORMCOUNT 97
/*
 * This size should really be based on the distance from the center of
 * rotation to the point on the object underneath the mouse.  That
 * point would then track the mouse as closely as possible.  This is a
 * simple example, though, so that is left as an Exercise for the
 * Programmer.
 */
#define TRACKBALLSIZE  (0.8)

float tb_project_to_sphere();

/*
 * Ok, simulate a track-ball.  Project the points onto the virtual
 * trackball, then figure out the axis of rotation, which is the cross
 * product of P1 P2 and O P1 (O is the center of the ball, 0,0,0)
 * Note:  This is a deformed trackball-- is a trackball in the center,
 * but is deformed into a hyperbolic solid of rotation away from the
 * center.
 * 
 * It is assumed that the arguments to this routine are in the range
 * (-1.0 ... 1.0)
 */
trackball(e,p1x,p1y,p2x,p2y)
float e[4], p1x, p1y, p2x, p2y;
{
    vect a;	/* Axis of rotation */
    float phi;	/* how much to rotate about axis */
    vect p1, p2, d;

    if (p1x == p2x && p1y == p2y) {
	vset4(e,0.0,0.0,0.0,1.0); /* Zero rotation */
	return;
    }

/*
 * First, figure out z-coordinates for projection of P1 and P2 to
 * deformed sphere
 */
    vset(&p1,p1x,p1y,tb_project_to_sphere(TRACKBALLSIZE,p1x,p1y));
    vset(&p2,p2x,p2y,tb_project_to_sphere(TRACKBALLSIZE,p2x,p2y));

/*
 *	Now, we want the cross product of P1 and P2
 */
    vcross(&p2,&p1,&a);

/*
 *	Figure out how much to rotate around that axis.
 */
    vsub(&p1,&p2,&d);
    phi = 2.0 * asin(vlength(&d) / (2.0*TRACKBALLSIZE));
    axis_to_quat(&a,phi,e);
}

/*
 *	Given an axis and angle, compute quaternion.
 */
axis_to_quat(a,phi,e)
vect *a;
float phi, e[4];
{
    vnormal(a);
    vcopy(a,e);
    vscale(e,fsin(phi/2.0));
    e[3] = fcos(phi/2.0);
}

/*
 * Project an x,y pair onto a sphere of radius r OR a hyperbolic sheet
 * if we are away from the center of the sphere.
 */
float tb_project_to_sphere(r,x,y)
float r, x, y;
{
    float d, t, z;

    d = fsqrt(x*x + y*y);
    if (d < r*SQRT1_2)  	/* Inside sphere */
	z = fsqrt(r*r - d*d);
    else { 			/* On hyperbola */
	t = r / SQRT2;
	z = t*t / d;
    }
    return z;
}

/*
 * Given two rotations, e1 and e2, expressed as quaternion rotations,
 * figure out the equivalent single rotation and stuff it into dest.
 * 
 * This routine also normalizes the result every RENORMCOUNT times it is
 * called, to keep error from creeping in.
 *
 */
add_quats(e1,e2,dest)
float e1[4], e2[4], dest[4];
{
    static int count=0;
    int i;
    float t1[4], t2[4], t3[4];
    float tf[4];

    vcopy(e1,t1); 
    vscale(t1,e2[3]);

    vcopy(e2,t2); 
    vscale(t2,e1[3]);

    vcross(e2,e1,t3);
    vadd(t1,t2,tf);
    vadd(t3,tf,tf);
    tf[3] = e1[3] * e2[3] - vdot(e1,e2);

    dest[0] = tf[0];
    dest[1] = tf[1];
    dest[2] = tf[2];
    dest[3] = tf[3];

    if (++count > RENORMCOUNT) {
	count = 0;
	normalize_quat(dest);
    }
}

/*
 * Quaternions always obey:  a^2 + b^2 + c^2 + d^2 = 1.0
 * If they don't add up to 1.0, dividing by their magnitued will
 * renormalize them.
 *
 * Note: See the following for more information on quaternions:
 * 
 * - Shoemake, K., Animating rotation with quaternion curves, Computer
 *   Graphics 19, No 3 (Proc. SIGGRAPH'85), 245-254, 1985.
 * - Pletinckx, D., Quaternion calculus as a basic tool in computer
 *   graphics, The Visual Computer 5, 2-13, 1989.
 */
normalize_quat(e)
float e[4];
{
	int i;
	float mag;

    mag = (e[0]*e[0] + e[1]*e[1] + e[2]*e[2] + e[3]*e[3]);
	for (i = 0; i < 4; i++) e[i] /= mag;
}

/*
 * Build a rotation matrix, given a quaternion rotation.
 *
 */
build_rotmatrix(m,e)
float m[4][4];
float e[4];
{
    m[0][0] = 1.0 - 2.0 * (e[1] * e[1] + e[2] * e[2]);
    m[0][1] = 2.0 * (e[0] * e[1] - e[2] * e[3]);
    m[0][2] = 2.0 * (e[2] * e[0] + e[1] * e[3]);
    m[0][3] = 0.0;

    m[1][0] = 2.0 * (e[0] * e[1] + e[2] * e[3]);
    m[1][1] = 1.0 - 2.0 * (e[2] * e[2] + e[0] * e[0]);
    m[1][2] = 2.0 * (e[1] * e[2] - e[0] * e[3]);
    m[1][3] = 0.0;

    m[2][0] = 2.0 * (e[2] * e[0] - e[1] * e[3]);
    m[2][1] = 2.0 * (e[1] * e[2] + e[0] * e[3]);
    m[2][2] = 1.0 - 2.0 * (e[1] * e[1] + e[0] * e[0]);
    m[2][3] = 0.0;

    m[3][0] = 0.0;
    m[3][1] = 0.0;
    m[3][2] = 0.0;
    m[3][3] = 1.0;
}

vcopy3(a,b)
float *a, *b;
{
    b[0] = a[0];
    b[1] = a[1];
    b[2] = a[2];
}

static float   idmat[4][4] = {
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0
};

myidentity()
{
    loadmatrix(idmat);
}

matinit()
{
    mmode(MVIEWING); 
    myidentity();
    smartsetdepth();
}
