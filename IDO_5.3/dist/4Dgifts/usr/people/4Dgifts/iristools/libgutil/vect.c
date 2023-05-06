/*
 * 	vect -
 *		Various functions to support operations on vectors.
 *
 *	David M. Ciemiewicz, Mark Grossman, Henry Moreton, and Paul Haeberli 
 *
 */
#include "vect.h"

vect *vnew()
{
    register vect *v;

    v = (vect *)malloc(sizeof(vect));
    return v;
}

vect *vclone(v)
vect *v;
{
    register vect *c;

    c = vnew();
    *c = *v;
    return c;
}

vcopy(v1,v2)
vect *v1, *v2;
{
    *v2 = *v1;
}

vprint(v)
vect *v;
{
    printf("x: %f y: %f z: %f\n",v->x,v->y,v->z);
}

vprint4(v)
vect *v;
{
    printf("x: %f y: %f z: %f w: %f\n",v->x,v->y,v->z,v->w);
}

vset(v,x,y,z)
vect *v;
float x, y, z;
{
    v->x = x;
    v->y = y;
    v->z = z;
}

vzero(v)
vect *v;
{
    v->x = 0.0;
    v->y = 0.0;
    v->z = 0.0;
    v->w = 0.0;
}

vone(v)
vect *v;
{
    v->x = 1.0;
    v->y = 1.0;
    v->z = 1.0;
    v->w = 1.0;
}

vset4(v,x,y,z,w)
vect *v;
float x, y, z, w;
{
    v->x = x;
    v->y = y;
    v->z = z;
    v->w = w;
}

vnormal(v)
vect *v;
{
    vscale(v,1.0/vlength(v));
}

float vlength(v) 
vect *v;
{
    return sqrt(v->x*v->x + v->y*v->y + v->z*v->z);

}

vscale(v,mul)
vect *v;
float mul;
{
    v->x *= mul;
    v->y *= mul;
    v->z *= mul;
}

vmult(src1,src2,dst)
vect *src1, *src2, *dst;
{
    dst->x = src1->x * src2->x;
    dst->y = src1->y * src2->y;
    dst->z = src1->z * src2->z;
}

vadd(src1,src2,dst)
vect *src1, *src2, *dst;
{
    dst->x = src1->x + src2->x;
    dst->y = src1->y + src2->y;
    dst->z = src1->z + src2->z;
}

vsub(src1,src2,dst)
vect *src1, *src2, *dst;
{
    dst->x = src1->x - src2->x;
    dst->y = src1->y - src2->y;
    dst->z = src1->z - src2->z;
}

vhalf(v1,v2,half)
vect *v1, *v2, *half;
{
    float len;

    vadd(v2,v1,half);
    len = vlength(half);
    if(len>0.0001)
	vscale(half,1.0/len);
    else
	*half = *v1;
}

float vdot(v1,v2) 
vect *v1, *v2;
{
    return v1->x*v2->x + v1->y*v2->y + v1->z*v2->z;
}

vcross(v1, v2, cross)
vect *v1, *v2, *cross;
{
    vect temp;

    temp.x = (v1->y * v2->z) - (v1->z * v2->y);
    temp.y = (v1->z * v2->x) - (v1->x * v2->z);
    temp.z = (v1->x * v2->y) - (v1->y * v2->x);
    *cross = temp;
}

vplane(normal, point, plane)
vect *normal, *point;
vect *plane;
{
    plane->x = normal->x;
    plane->y = normal->y;
    plane->z = normal->z;
    plane->w = - vdot(normal, point);
}

vdirection(v1, dir)
vect *v1, *dir;
{
	*dir = *v1;
	vnormal(dir);
}

makeplane(p1, p2, p3, v)
vect *p1, *p2, *p3, *v;
{
    vect a, b;

    vsub(p1,p2,&a);
    vsub(p3,p2,&b);
    vcross(&a,&b,&b);
    vnormal(&b);
    vplane(&b,p1,v);
}

vreflect(in,mirror,out)
vect *in, *mirror, *out;
{
    vect temp;

    temp = *mirror;
    vscale(&temp,vdot(mirror,in));
    vsub(&temp,in,out);
    vadd(&temp,out,out);
}

vmultmatrix(m1,m2,prod)
float m1[4][4], m2[4][4], prod[4][4];
{
    register int row, col;
    float temp[4][4];

    for(row=0 ; row<4 ; row++) 
        for(col=0 ; col<4 ; col++)
            temp[row][col] = m1[row][0] * m2[0][col]
                           + m1[row][1] * m2[1][col]
                           + m1[row][2] * m2[2][col]
                           + m1[row][3] * m2[3][col];
    for(row=0 ; row<4 ; row++) 
        for(col=0 ; col<4 ; col++)
	    prod[row][col] = temp[row][col];
}

vtransform(v,mat,vt)
vect *v;
float mat[4][4];
vect *vt;
{
    vect t;

    t.x = v->x*mat[0][0] + v->y*mat[1][0] + v->z*mat[2][0] + v->w*mat[3][0];
    t.y = v->x*mat[0][1] + v->y*mat[1][1] + v->z*mat[2][1] + v->w*mat[3][1];
    t.z = v->x*mat[0][2] + v->y*mat[1][2] + v->z*mat[2][2] + v->w*mat[3][2];
    t.w = v->x*mat[0][3] + v->y*mat[1][3] + v->z*mat[2][3] + v->w*mat[3][3];
    *vt = t;
}

vlerp(v0,v1,v,p)
vect *v0, *v1, *v;
float p;
{
    v->x = flerp(v0->x,v1->x,p);
    v->y = flerp(v0->y,v1->y,p);
    v->z = flerp(v0->z,v1->z,p);
    v->w = flerp(v0->w,v1->w,p);
}

float flerp(f0,f1,p)
float f0, f1, p; 
{
    return ((f0*(1.0-p))+(f1*p));
}

lerp(i0,i1,p)
int i0, i1;
float p; 
{
    return ((i0*(1.0-p))+(i1*p));
}

float frand();

vperturb(v,p,mag)
vect *v, *p;
float mag;
{
    int dir;
    vect t;
    float m;

    do  {
	p->x = v->x + mag*(frand()-0.5);
	p->y = v->y + mag*(frand()-0.5);
	p->z = v->z + mag*(frand()-0.5);
	m = vlength(p);
    } while (m<0.01);
    p->x /= m;
    p->y /= m;
    p->z /= m;
}

int trinormal(p00,p01,p10,n,tol)
vect *p00,*p01,*p10,*n;
float tol;
{
    double tx, ty, tz;
    double Xj, Yj, Zj;
    double Xi, Yi, Zi;
    double mag;

    Xj = p10->x;
    Yj = p10->y;
    Zj = p10->z;
    tx = ty = tz = 0.0;

    Xi = Xj; Xj = p00->x;
    Yi = Yj; Yj = p00->y;
    Zi = Zj; Zj = p00->z;
    tx += (Yi - Yj) * (Zi + Zj);
    ty += (Zi - Zj) * (Xi + Xj);
    tz += (Xi - Xj) * (Yi + Yj);
	
    Xi = Xj; Xj = p01->x;
    Yi = Yj; Yj = p01->y;
    Zi = Zj; Zj = p01->z;
    tx += (Yi - Yj) * (Zi + Zj);
    ty += (Zi - Zj) * (Xi + Xj);
    tz += (Xi - Xj) * (Yi + Yj);
	
    Xi = Xj; Xj = p10->x;
    Yi = Yj; Yj = p10->y;
    Zi = Zj; Zj = p10->z;
    tx += (Yi - Yj) * (Zi + Zj);
    ty += (Zi - Zj) * (Xi + Xj);
    tz += (Xi - Xj) * (Yi + Yj);

    mag = sqrt(tx*tx + ty*ty + tz*tz);
    if( mag < tol) {
	n->x = 0.0;
	n->y = 0.0;
	n->z = 0.0;
	return 0;
    } else {
	n->x = tx/mag;
	n->y = ty/mag;
	n->z = tz/mag;
	return 1;
    }
}
