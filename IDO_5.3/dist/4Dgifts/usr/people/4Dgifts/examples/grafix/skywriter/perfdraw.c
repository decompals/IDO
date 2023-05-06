
/*
 * perfdraw.c - $Revision: 1.7 $
*/

#include <gl/gl.h>
#include <math.h>
#include <stdio.h>
#include "skyfly.h"

/* static routine decls */

extern int MS;

static void drawlitmesh_11(float *objdata); 
static void drawcolrtexmesh_10(float *objdata); 
static void drawclouds(float *objdata);

static float    IdentMat[4][4] = {
    1., 0., 0., 0.,
    0., 1., 0., 0.,
    0., 0., 1., 0.,
    0., 0., 0., 1.
};


void drawperfobj(perfobj_t *perfobj)
{
    int        i, n, j;
    float     *vdata_ptr =(float *) perfobj->vdata;

    register unsigned long *flagsptr = perfobj->flags;
    register float     *dp;

    while (1) {
	switch (*flagsptr) {

	/*
	 * A paper plane is a single tmesh folded on itself so the orientations
	 * of some triangles in the mesh are incorrect with respect to 
	 * their normals. This is solved by drawing the tmesh twice;
	 * first draw only backfaces, then only frontfaces.
	*/
	case PD_DRAW_PAPER_PLANE:
	    flagsptr += 1;
	    frontface(TRUE);
	    backface(FALSE);
	    drawlitmesh_11(vdata_ptr);
	    backface(TRUE);
	    frontface(FALSE);
	    drawlitmesh_11((float *)
			      ((perfobj_vert_t *) vdata_ptr + 11));
	    popmatrix();
	    break;

	case PD_DRAW_TERRAIN_CELL:
	    dp = *(float **) (flagsptr + 1);
	    flagsptr += 2;
	    drawcolrtexmesh_10(dp);
	    break;

	case PD_DRAW_CLOUDS:
	    cpack(0xb04030);
	    backface(FALSE);
	    texbind(0, 2);
	    tevbind(0, 2);
	    drawclouds(vdata_ptr);
	    backface(TRUE);
	    flagsptr += 1;
	    break;

	case PD_PAPER_PLANE_MODE:
	    switch (*(flagsptr + 1)) {
	    case PLANES_START:
		lmbind(LMODEL, 1);
		shademodel(FLAT);
		tevbind(0, 0);	    /* Turn texturing off */
		break;
	    /*
	     * The paper planes are drawn a second time as anti-aliased 
	     * lines. Thus, the plane tmesh is 'outlined' with smooth lines 
	     * and appears anti-aliased.
	    */
	    case PLANES_SECOND_PASS:
		blendfunction(BF_SA, BF_MSA);
		linesmooth(SML_SMOOTHER | SML_ON);
		polymode(PYM_LINE_FAST);
		break;
	    case PLANES_END:
		if (!MS) {
		blendfunction(BF_ONE, BF_ZERO);
		polymode(PYM_FILL);
		backface(TRUE);
		frontface(FALSE);
		}
		shademodel(GOURAUD);
		lmbind(LMODEL, 0);
		break;
	    }
	    flagsptr += 2;
	    break;

	case PD_PAPER_PLANE_POS:	/* contains the pushmatrix */
	    pushmatrix();
	    translate(*(vdata_ptr), *(vdata_ptr + 1), *(vdata_ptr + 2));
	    rot(*(vdata_ptr + 3), 'z');
	    rot(*(vdata_ptr + 4), 'y');
	    rot(*(vdata_ptr + 5), 'x');
	    flagsptr += 1;
	    break;

	case PD_VIEWER_POS:
	    loadmatrix(IdentMat);
	    rot(-90, 'x');
	    rot(*(vdata_ptr + 3) * RAD_TO_DEG, 'z');	/* yaw */
	    lmbind(LIGHT0, 1);
	    translate(-*(vdata_ptr), -*(vdata_ptr + 1), -*(vdata_ptr + 2));
	    flagsptr += 1;
	    break;

	case PD_TEXTURE_BIND:
	    texbind(0, *(flagsptr + 1));
	    tevbind(0, 1);
	    flagsptr += 2;
	    break;

	case PD_END:
	    return;

	default:
	    fprintf(stderr, "Bad PD primitive %d\n", *flagsptr);
	    flagsptr++;
	    break;
	}
    }
}

/*
 * Notice how the following routines unwind loops and pre-compute indexes
 * at compile time. This is crucial in obtaining the maximum data transfer 
 * from cpu to the graphics pipe.
*/
static void 
drawlitmesh_11(float *op) 
{
    bgntmesh();
    /* one */
    n3f((op + PD_V_NORMAL));
    v3f((op + PD_V_POINT));
    /* two */
    n3f((op + (PD_V_SIZE + PD_V_NORMAL)));
    v3f((op + (PD_V_SIZE + PD_V_POINT)));
    /* three */
    n3f((op + (2 * PD_V_SIZE + PD_V_NORMAL)));
    v3f((op + (2 * PD_V_SIZE + PD_V_POINT)));
    /* four */
    n3f((op + (3 * PD_V_SIZE + PD_V_NORMAL)));
    v3f((op + (3 * PD_V_SIZE + PD_V_POINT)));
    /* five */
    n3f((op + (4 * PD_V_SIZE + PD_V_NORMAL)));
    v3f((op + (4 * PD_V_SIZE + PD_V_POINT)));
    /* six */
    n3f((op + (5 * PD_V_SIZE + PD_V_NORMAL)));
    v3f((op + (5 * PD_V_SIZE + PD_V_POINT)));
    /* seven */
    n3f((op + (6 * PD_V_SIZE + PD_V_NORMAL)));
    v3f((op + (6 * PD_V_SIZE + PD_V_POINT)));
    /* eight */
    n3f((op + (7 * PD_V_SIZE + PD_V_NORMAL)));
    v3f((op + (7 * PD_V_SIZE + PD_V_POINT)));
    /* nine */
    n3f((op + (8 * PD_V_SIZE + PD_V_NORMAL)));
    v3f((op + (8 * PD_V_SIZE + PD_V_POINT)));
    /* ten */
    n3f((op + (9 * PD_V_SIZE + PD_V_NORMAL)));
    v3f((op + (9 * PD_V_SIZE + PD_V_POINT)));
    /* eleven */
    n3f((op + (10 * PD_V_SIZE + PD_V_NORMAL)));
    v3f((op + (10 * PD_V_SIZE + PD_V_POINT)));

    endtmesh();

}

static void 
drawcolrtexmesh_10(float *op) 
{
    bgntmesh();
    /* one */
    t2f((op + PD_V_TEX));
    c3f((op + PD_V_COLOR));
    v3f((op + PD_V_POINT));
    /* two */
    t2f((op + (PD_V_SIZE + PD_V_TEX)));
    c3f((op + (PD_V_SIZE + PD_V_COLOR)));
    v3f((op + (PD_V_SIZE + PD_V_POINT)));
    /* three */
    t2f((op + (2 * PD_V_SIZE + PD_V_TEX)));
    c3f((op + (2 * PD_V_SIZE + PD_V_COLOR)));
    v3f((op + (2 * PD_V_SIZE + PD_V_POINT)));
    /* four */
    t2f((op + (3 * PD_V_SIZE + PD_V_TEX)));
    c3f((op + (3 * PD_V_SIZE + PD_V_COLOR)));
    v3f((op + (3 * PD_V_SIZE + PD_V_POINT)));
    /* five */
    t2f((op + (4 * PD_V_SIZE + PD_V_TEX)));
    c3f((op + (4 * PD_V_SIZE + PD_V_COLOR)));
    v3f((op + (4 * PD_V_SIZE + PD_V_POINT)));
    /* six */
    t2f((op + (5 * PD_V_SIZE + PD_V_TEX)));
    c3f((op + (5 * PD_V_SIZE + PD_V_COLOR)));
    v3f((op + (5 * PD_V_SIZE + PD_V_POINT)));
    /* seven */
    t2f((op + (6 * PD_V_SIZE + PD_V_TEX)));
    c3f((op + (6 * PD_V_SIZE + PD_V_COLOR)));
    v3f((op + (6 * PD_V_SIZE + PD_V_POINT)));
    /* eight */
    t2f((op + (7 * PD_V_SIZE + PD_V_TEX)));
    c3f((op + (7 * PD_V_SIZE + PD_V_COLOR)));
    v3f((op + (7 * PD_V_SIZE + PD_V_POINT)));
    /* nine */
    t2f((op + (8 * PD_V_SIZE + PD_V_TEX)));
    c3f((op + (8 * PD_V_SIZE + PD_V_COLOR)));
    v3f((op + (8 * PD_V_SIZE + PD_V_POINT)));
    /* ten */
    t2f((op + (9 * PD_V_SIZE + PD_V_TEX)));
    c3f((op + (9 * PD_V_SIZE + PD_V_COLOR)));
    v3f((op + (9 * PD_V_SIZE + PD_V_POINT)));

    endtmesh();

}

static void 
drawclouds(float *op)
{
    bgnpolygon();
    /* one */
    t2f((op + PD_V_TEX));
    v3f((op + PD_V_POINT));
    /* two */
    t2f((op + (PD_V_SIZE + PD_V_TEX)));
    v3f((op + (PD_V_SIZE + PD_V_POINT)));
    /* three */
    t2f((op + (2 * PD_V_SIZE + PD_V_TEX)));
    v3f((op + (2 * PD_V_SIZE + PD_V_POINT)));
    /* four */
    t2f((op + (3 * PD_V_SIZE + PD_V_TEX)));
    v3f((op + (3 * PD_V_SIZE + PD_V_POINT)));

    endpolygon();

}

void 
putv3fdata(float *v, perfobj_vert_t *ptr)
{
  ptr->vert[0] = v[0];
  ptr->vert[1] = v[1];
  ptr->vert[2] = v[2];
}

void 
putc3fdata(float *c, perfobj_vert_t *ptr)
{
  ptr->color[0] = c[0];
  ptr->color[1] = c[1];
  ptr->color[2] = c[2];
}

void 
putn3fdata(float *n, perfobj_vert_t *ptr)
{
  ptr->normal[0] = n[0];
  ptr->normal[1] = n[1];
  ptr->normal[2] = n[2];
}

void 
putt2fdata(float *t, perfobj_vert_t *ptr)
{
  ptr->texture[0] = t[0];
  ptr->texture[1] = t[1];
}

