/*
 *   upat.c:
 *
 *    This module contains all the code necessary to hook the routine upat()
 *  into any application wanting to take advantage of "user-defined up-axis"
 *  functionality.  Excluding its seventh parameter--twist--upat() can be 
 *  thought of as a superset of the LOOKAT(3G) function (the first six 
 *  parameters are identical to the first six parameters of LOOKAT(3G)).  
 *
 * C SPECIFICATION
 *     void upat(vx, vy, vz, px, py, pz, ux, uy, uz)
 *     float vx, vy, vz, px, py, pz, ux, uy, uz;
 *
 * PARAMETERS
 *     vx, vy, vz   is the viewpoint, the point the eye is looking from
 *     px, py, pz   is the reference point, the point the eye is looking at
 *     ux, uy, uz   is the up vector
 */

#include <stdio.h>
#include <math.h>
#include <gl/gl.h>

#define X 0
#define Y 1
#define Z 2

void normalize(float *);
void cross(float *result, float *v1, float *v2);

void upat(float vx, float vy, float vz, 
          float px, float py, float pz, 
          float ux, float uy, float uz) 
{
    int i;
    float forward[3], side[3], up[3];
    float m[4][4];


    forward[X] = px - vx;
    forward[Y] = py - vy;
    forward[Z] = pz - vz;

    /* temporarily use view-up to hold world-up */
    up[X] = ux;	    
    up[Y] = uy;
    up[Z] = uz;

    normalize(forward);

    /* generate the vector side from the
     * 2 vectors view forward and world up 
     */
    cross(side, forward, up);
    normalize(side);

    /* generate the view up vector from 
     * the 2 vectors view forward and view side 
     */
    cross(up, side, forward);

    m[0][0] = side[X];
    m[1][0] = side[Y];
    m[2][0] = side[Z];
    m[3][0] = 0.0;

    m[0][1] = up[X];
    m[1][1] = up[Y];
    m[2][1] = up[Z];
    m[3][1] = 0.0;

    m[0][2] = -forward[X];
    m[1][2] = -forward[Y];
    m[2][2] = -forward[Z];
    m[3][2] = 0.0;

    m[0][3] = 0.0;
    m[1][3] = 0.0;
    m[2][3] = 0.0;
    m[3][3] = 1.0;

    multmatrix(m);
    translate(-vx, -vy, -vz);
}



/* 
 *   generate a normalized vector of length 1.0
 */
void normalize(float *v) 
{
    float r;


    r = sqrt( v[X]*v[X] + v[Y]*v[Y] + v[Z]*v[Z] );

    v[X] /= r;
    v[Y] /= r;
    v[Z] /= r;
}



/*
 *   make a cross product of the two vectors passed in via v1 and v2,
 *   and return the resulting perpendicular-to-both vector in result
 */
void cross(float *result, float *v1, float *v2) 
{
    result[X] = v1[Y]*v2[Z] - v1[Z]*v2[Y];
    result[Y] = v1[Z]*v2[X] - v1[X]*v2[Z];
    result[Z] = v1[X]*v2[Y] - v1[Y]*v2[X];
}
