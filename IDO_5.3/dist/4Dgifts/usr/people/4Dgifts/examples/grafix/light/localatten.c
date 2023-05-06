/* 
 *   localatten.c:
 *
 *     This program draws a flat green plate at z = 0; -1.0 <= x, y <= 1.0.
 *  The eye is 6 units above, looking down.  A light bounces up and down in 
 *  the range 0.1 <= z <= 1.5, and x = y = 0.  The lighting model attenuates
 *  intensity with  distance, so the center of the plate gets brighter as 
 *  the light moves closer.  The character string printed at the lower left 
 *  of the plate shows the height of the light.  Note that the color is set 
 *  after the cmov() command -- the cmov() actually sends a vertex through 
 *  the transformation, and it will set the current color.  If you move the 
 *  cpack() command just above the cmov() command, the character string will 
 *  be lighted and will appear in varying shades of green.  
 *
 */ 
#include <stdio.h> 
#include <gl/gl.h> 
#include <gl/device.h> 

Matrix idmat = {1.0,0.0,0.0,0.0, 
                0.0,1.0,0.0,0.0, 
                0.0,0.0,1.0,0.0, 
                0.0,0.0,0.0,1.0};

float  green_material[] = {DIFFUSE,  0.0, 1.0, 0.0,
			   LMNULL};

float  local_white_light[] = {LCOLOR, 1.0, 1.0, 1.0,
		              POSITION, 0.0, 0.0, 1.0, 1.0,
		              LMNULL};

float  light_model[] = {AMBIENT, 0.0, 0.0, 0.0,
			LOCALVIEWER, 0.0,
			ATTENUATION, 1.0, 1.0,
			LMNULL};

/*
** draw_plate draws a flat plate covering the
** range -1.0 <= x, y <= 1.0 and z = 0. using
** n^2 rectangles.  All the normal vectors are
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
** diffuse and ambient reflection.  In
** addition, this lighting calculation
** includes a local light whose emitted
** light is attenuated as a function of
** distance from the object.
*/
def_light_calc()
{
    lmdef(DEFLMODEL, 1, 10, light_model);
    lmdef(DEFMATERIAL, 1, 5, green_material);
    lmdef(DEFLIGHT, 1, 10, local_white_light);
}

/*
** Tell the Graphics Library to USE the lighting
** calculation that we defined earlier.
*/
use_light_calc()
{
    lmbind(LMODEL, 1);
    lmbind(LIGHT1, 1);
    lmbind(MATERIAL, 1);
}

main()
{
    float dist;
    long flag = 1;
    char str[32];
    short val;

    keepaspect(1, 1);
    minsize(250,250);
    winopen("local");
    RGBmode();
    doublebuffer();
    gconfig();
    qdevice(ESCKEY);

    /*
    ** Use mmode() to set up projection and
    ** viewing matrices for lighting.
    */    
    mmode(MVIEWING);
    perspective(400, 1.0, 0.5, 10.0);
    loadmatrix(idmat);
    lookat(0.0,0.0,6.0,0.0,0.0,0.0,0);

    def_light_calc();
    use_light_calc();

    dist = 1.0;

    while (TRUE) {
	if (flag) {
	    dist += .01;
	    if (dist > 1.5) flag = 1 - flag;
	} else {
	    dist -= .01;
	    if (dist < 0.1) flag = 1 - flag;
	}
	cpack(0);
        clear();
	sprintf(str, "Light Distance: %1.2f", dist);
	cmov2(-1.5, -1.5);
	cpack(0xffffff);
	charstr(str);
	pushmatrix();
	    /*
	    ** Change the position of the local light
	    ** by REDEFINING and REBINDING the light.
	    ** Repositioning the light changes the
	    ** illumination of the plate for two reasons:
	    ** 1) the affect of attenuation, and 
	    ** 2) the light direction vector from a
	    ** vertex on the plate to the repositioned
	    ** light source has changed.
	    */
	    local_white_light[7] = dist;
	    lmdef(DEFLIGHT, 1, 10, local_white_light);
	    lmbind(LIGHT1, 1);
            draw_plate(20);
        popmatrix();
	swapbuffers();
	if (qtest()) {
	   switch(qread(&val)) {
	       case ESCKEY:
		   exit(0);
		   break;
	       case REDRAW:
		   reshapeviewport();
		   break;
	    }
        }
    }
}
