/*
 *	glshade -
 *		Simple support for describing materials and light sources 
 *	This makes it easy for people without PHD's in GL to actually 
 *	shade surfaces.
 *
 *				Paul Haeberli - 1990
 *
 *  exports:
 *	    matrixinit();
 *	    shadeinit();
 *	    shadeon();
 *	    shadeoff();
 *	    setdiffuse(r,g,b);
 *	    setspecular(r,g,b);
 *	    setshininess(s);
 *
 *
 *  Here is the structure of a typical program:
 *
 *	    main() 
 *	    {
 *		winopen("shade");
 *		RGBmode();
 *		doublebuffer();
 *		gconfig();
 *		winopen("blat");
 *		matrixinit();		    you must use two matrix model 
 *		.
 *		.
 *		perspective(  . . . . );
 *		shadeinit();		    init shading after perspective 
 *		.
 *		.
 *		while(1)
 *		    cpack(0x00404060);	    clear the screen
 *		    clear();
 *		    zclear();
 *		    .
 *		    .
 *		    shadeon();			turn lighting on
 *	    	    setdiffuse(1.0,0.0,0.0);	make the diffuse color red
 *		    drawobjwithnormals();	draw the object
 *		    shadeoff();			turn lighting off
 *		    .
 *		    .
 *		    swapbuffers();	    swap buffers
 *	 	}
 *	    }
 */
#include "stdio.h"
#include "gl.h"

static initmaterial();
static initlights();
static makelight();

static float   idmat[4][4] = {
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0
};

matrixinit()
{
    mmode(MVIEWING); 
    loadmatrix(idmat);
}

shadeinit()
{
    initmaterial();
    initlights();
    shadeoff();
}

shadeon()
{
    lmbind(LMODEL,1);
}

shadeoff()
{
    lmbind(LMODEL,0);
}

setdiffuse(r,g,b)
float r, g, b;
{
    float c[3];

    lmcolor(LMC_DIFFUSE);
    c[0] = r;
    c[1] = g;
    c[2] = b;
    c3f(c);
    lmcolor(LMC_COLOR);
}

setspecular(r,g,b)
float r, g, b;
{
    float c[3];

    lmcolor(LMC_SPECULAR);
    c[0] = r;
    c[1] = g;
    c[2] = b;
    c3f(c);
    lmcolor(LMC_COLOR);
}

setshininess(s)
float s;
{
    float mat_desc[3];

    mat_desc[0] = SHININESS;
    mat_desc[1] = s;
    mat_desc[2] = LMNULL;
    lmdef(DEFMATERIAL,1,3,mat_desc);
}


static initmaterial()
{
    float mat_desc[19];

    mat_desc[0] = EMISSION;
    mat_desc[1] = 0.0;
    mat_desc[2] = 0.0;
    mat_desc[3] = 0.0;
    mat_desc[4] = AMBIENT;
    mat_desc[5] = 0.0;
    mat_desc[6] = 0.0;
    mat_desc[7] = 0.0;
    mat_desc[8] = DIFFUSE;
    mat_desc[9] = 1.0;
    mat_desc[10] = 1.0;
    mat_desc[11] = 1.0;
    mat_desc[12] = SPECULAR;
    mat_desc[13] = 0.9;
    mat_desc[14] = 0.9;
    mat_desc[15] = 0.9;
    mat_desc[16] = SHININESS;
    mat_desc[17] = 40.0;
    mat_desc[18] = LMNULL;
    lmdef(DEFMATERIAL,1,19,mat_desc);
    lmbind(MATERIAL,1);
}

static initlights()
{
    float li_desc[10];

    li_desc[0] = AMBIENT;
    li_desc[1] = 0.10;
    li_desc[2] = 0.10;
    li_desc[3] = 0.10;
    li_desc[4] = LOCALVIEWER;
    li_desc[5] = 0.0;
    li_desc[6] = ATTENUATION;
    li_desc[7] = 1.0;
    li_desc[8] = 0.0;
    li_desc[9] = LMNULL;
    lmdef(DEFLMODEL,1,10,li_desc);
    lmbind(LMODEL,1);

    makelight(0,-3.0,3.0,10.0);
}

static makelight(i,x,y,z)
int i;
float x, y, z;
{
    float li_desc[14];

    li_desc[0] = AMBIENT; 
    li_desc[1] = 0.1; 
    li_desc[2] = 0.1;
    li_desc[3] = 0.1;
    li_desc[4] = LCOLOR;
    li_desc[5] = 1.0;
    li_desc[6] = 1.0;
    li_desc[7] = 1.0;
    li_desc[8] = POSITION;
    li_desc[9] = x;
    li_desc[10] = y;
    li_desc[11] = z;
    li_desc[12] = 0.0;		/* zero means infinte light */
    li_desc[13] = LMNULL;
    lmdef(DEFLIGHT,i+1,14,li_desc);
    lmbind(LIGHT0+i,i+1);
}
