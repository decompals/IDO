/*
 *    platelocal.c:
 *
 *     This program draws a flat plate with a simple local light.  If the 
 *  line at the top of the file is left in, the light is fixed, and the 
 *  plate moves.  Thus the bright spot on the plate will appear to move 
 *  around (on the plate).  Sometimes, the plate gets in front of the light,
 *  and almost disappears, since only the back is lit.  It does not quite 
 *  disappear, since there is a small ambient component for the default 
 *  material.
 * 
 *     If the top line is deleted, the light is effectively attached to the 
 *  moving plate, so the lighted portion of the plate moves with the plate.
 *
 */

#include <gl.h>
#include <stdio.h>

#define FIXED_LIGHT 

Matrix idmat = {1.0,0.0,0.0,0.0,
                0.0,1.0,0.0,0.0,
		0.0,0.0,1.0,0.0,
		0.0,0.0,0.0,1.0};

float  white_material[] =   {DIFFUSE,  1.0, 1.0, 1.0,
			     SPECULAR, 0.0, 0.0, 0.0,
			     LMNULL};

float  local_blue_light[] = {LCOLOR, 0.0, 0.0, 1.0,
		             POSITION, 0.5, 0.5, 0.1, 1.0,
		             LMNULL};

/*
** draw_plate draws a flat plate covering
** the range -1.0 <= x, y <= 1.0 and z = 0.0
** using n^2 rectangles.  All the normal vectors are
** perpendicular to the plate.
*/

draw_plate(n)
long n;	
{
    long i, j;
    float p0[3], p1[3], p2[3], p3[3];
    float n0[3];

    n0[0] = n0[1] = 0.0; n0[2] = 1.0;
    p0[2] = p1[2] = p2[2] = p3[2] = 0.0;

    for (i = 0; i < n; i++) {
	p0[0] = p1[0] = -1.0 + 2.0*i/n;
	p2[0] = p3[0] = -1.0 + 2.0*(i+1)/n;
	for (j = 0; j < n; j++) {
	    p0[1] = p3[1] = -1.0 + 2.0*j/n;
	    p1[1] = p2[1] = -1.0 + 2.0*(j+1)/n;
	    bgnpolygon();
	    n3f(n0); v3f(p0);
	    n3f(n0); v3f(p1);
	    n3f(n0); v3f(p2);
	    n3f(n0); v3f(p3);
	    endpolygon();
	}
    }
}

/*
** Tell the Graphics Library to DEFINE a
** lighting calculation that accounts for
** diffuse and ambient reflection.  In addition,
** the lighting calculation includes a LOCAL light.
*/
def_light_calc()
{
    lmdef(DEFLMODEL, 1, 0, NULL);
    lmdef(DEFMATERIAL, 1, 9, white_material);
    lmdef(DEFLIGHT, 1, 10, local_blue_light);
}

/*
** Tell the Graphics Library to USE the lighting
** calculation that we defined earlier.
*/
use_light_calc()
{
    lmbind(LMODEL, 1);
    lmbind(LIGHT0, 1);
    lmbind(MATERIAL, 1);
}

main()
{
    int i;

    keepaspect(1, 1);
    winopen("local");
    RGBmode();
    doublebuffer();
    gconfig();

    /*
    ** Use mmode() to set up projection and viewing
    ** matrices for lighting.
    */    
    mmode(MVIEWING);
    perspective(400, 1.0, 0.5, 10.0);
    loadmatrix(idmat);
    lookat(0.0,0.0,6.0,0.0,0.0,0.0,0);

    def_light_calc();
    use_light_calc();

    for (i = 0; i < 1800; i++) {
	cpack(0);
        clear();
	pushmatrix();
	    rot(i*0.5, 'Z'); 
	    rot(i*0.5, 'Y'); 
#ifndef FIXED_LIGHT
	    lmbind(LIGHT0, 1);
#endif FIXED_LIGHT
            draw_plate(10);
        popmatrix();
	swapbuffers();
    }
}
