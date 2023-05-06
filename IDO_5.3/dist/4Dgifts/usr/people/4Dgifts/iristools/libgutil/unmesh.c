/*
 *	unmesh -
 *		Convert tmesh calls into triangles.
 *
 *			Paul Haeberli - 1990
 */
#include "stdio.h"

#define V_NULL			0
#define V_FIRSTTMESH		1
#define V_SECONDTMESH		2
#define V_THIRDTMESH		3
#define V_TMESH			4

static savemesh();
static meshout_bgnpoly();
static meshout_vert();
static meshout_endpoly();


static int vmode = V_NULL;
static int replaceb;
static float *a, *b;
static int (*meshcallback)();
static int meshverts;

/*
 *	Input functions
 *
 */
mesh_callback(func)
int (*func)();
{
    meshcallback = func;
}

mesh_bgntmesh()
{
    vmode = V_FIRSTTMESH;
}

mesh_endtmesh()
{
    switch (vmode) {
	case V_SECONDTMESH:
	case V_THIRDTMESH:
	    meshout_endpoly();
	    break;
	default: break;
    }
    vmode = V_NULL;
}

mesh_swaptmesh()
{
    replaceb = !replaceb;
}

mesh_vert(v)
float *v;
{
    register float x,y,z;

    switch(vmode) {
	case V_FIRSTTMESH:
	    meshout_bgnpoly();
	    meshout_vert(v);
	    replaceb = 0;
	    savemesh(v);
	    vmode = V_SECONDTMESH;
	    break;
	case V_SECONDTMESH:
	    meshout_vert(v);
	    savemesh(v);
	    vmode = V_THIRDTMESH;
	    break;
	case V_THIRDTMESH:
	    meshout_vert(v);
	    meshout_endpoly();
	    savemesh(v);
	    vmode = V_TMESH;
	    break;
	case V_TMESH:
	    meshout_bgnpoly();
	    meshout_vert(v);
	    meshout_vert(a);
	    meshout_vert(b);
	    meshout_endpoly();
	    savemesh(v);
	    break;
    }
}					 
					 
static savemesh(v)
float *v;
{
    if (!replaceb)
	a = v;
    else 
	b = v;
    replaceb = !replaceb;
}

/*
 *    output functions 
 *
 */
static int meshpolyp;
static long meshvert[3];

static meshout_bgnpoly()
{
    meshpolyp = 0;
}

static meshout_vert(ptr)
long ptr;
{
    float x, y, z;
    int offset;

    meshvert[meshpolyp] = ptr;
    meshpolyp++;
    if(meshpolyp > 3) {
	fprintf(stderr,"meshout_vert: too many verticies\n");
	exit(1);
    }
}

static meshout_endpoly()
{
    if(meshpolyp != 3) {
	fprintf(stderr,"meshout_endpoly: vert count not 3\n");
	exit(1);
    }
    if(!meshcallback) {
	fprintf(stderr,"meshout_endpoly: mesh call back not defined\n");
	exit(1);
    }
    (meshcallback)(meshvert);
}
