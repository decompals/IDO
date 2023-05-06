/*
 *
 *    The following program is a modification of a sample program from the 
 *  GT Graphics Library User's Guide, Chapter 4 and illustrates how to use 
 *  the Graphics Library to perform lighting for a cylinder with the simple
 *  lighting calculation explained at the beginning of chapter 9.
 *
 *    JULY 1990 - Modified again to show two sided lighting.
 *         Pressing the LEFTMOUSE button will use the default
 *	   material of the back side, ie. the same as the
 *	   front side. Releasing the LEFTMOUSE button allows
 *	   a different material to be used.
 *			
 *						Martin R. McDonald
 *						SGI
 *					
 *
 */
#include <gl.h>
#include <device.h>
#include <math.h>
#include <stdio.h>

#define RADIUS 0.9
#define TWOPI 6.28318530
#define PI 3.14159265

/* define black RGB color */
float  blackvec[3] = {0.0, 0.0, 0.0};	
Matrix idmat = {1.0,0.0,0.0,0.0, /* identity matrix */
                0.0,1.0,0.0,0.0,
		0.0,0.0,1.0,0.0,
		0.0,0.0,0.0,1.0};

/*define a polygon with some structures
 * -- for code readability*/
typedef struct { /* structure for a 3-D vertex */
    Coord x;
    Coord y;
    Coord z;
} POINT;

typedef struct { /* 4 vertex lighted polygon struct */
    POINT vertex[4];
    POINT normal[4];
} POLYGON;

int number_of_polys;	/* cylinder polygon count */
POLYGON *polygon;	/* pointer to polygon list */

/*
** add TWOSIDE to the light model
*/
float two[] = { TWOSIDE, 1, LMNULL };

/*
** define a BACKMATERIAL
*/
float back[] =
	{SPECULAR,  0.3, 0.3, 0.3, /* gray reflectance */
	 DIFFUSE,   0.8, 0.0, 0.8, /* purple reflectance */
	 SHININESS, 3.0,           /* unfocused highlight */
	 AMBIENT,   0.2,0.0,0.2,   /* purple reflectance */
	 LMNULL};



/*
** def_simple_light_calc()
** Tell the Graphics Library to DEFINE a simple
** lighting calculation that accounts for diffuse
** and ambient reflection.  This simple
** lighting calculation happens to use the default
** lighting parameters in the Graphics Library, with
** two exceptions. Two-sided lighting is added to the
** light model definition. Pressing the LEFTMOUSE
** button will toggle using the default BACKMATERIAL
** and a user defined BACKMATERIAL.
*/ 

def_simple_light_calc() { 
    lmdef(DEFMATERIAL, 1, 0, NULL);
    lmdef(DEFMATERIAL, 2, 15, back);
    lmdef(DEFLIGHT, 1, 0, NULL);
    lmdef(DEFLMODEL, 1, 3, two);
}

/*
** use_simple_light_calc()
** Tell the Graphics Library to USE the
** lighting calculation that we 
** defined earlier.
*/
use_simple_light_calc()
{
    lmbind(MATERIAL, 1);
    lmbind(BACKMATERIAL, 2);
    lmbind(LIGHT0, 1);
    lmbind(LMODEL, 1);
}

/* 
** make_cylinder()
** Draw a cylinder using (2 * n) polygons
** to approximate the curvature and n 
** polygons to describe the length.
** This requires (2 * n^2) polygons to
** describe the cylinder. Compute the
** surface normal at each vertex so we
** can use the Graphics Library to perform
** lighting calculations.
*/
make_cylinder(n)
int n;
{
    POLYGON *p;		/* pointer into polygon list */
    float theta, dtheta,/* current angle and angle */
			/*increment around section */
	  x, dx;	/* current position and */
			/* increment along cylinder side */
    int vertex_i;	/* vertex counter */

    /* allocate and point to enough */
    /* memory for all the polygons */
    number_of_polys = 2 * n * n;
    p = polygon = (POLYGON *)
	malloc(number_of_polys * sizeof(POLYGON));

    dx = 3.0/n;	    /* n polygons for 3.0 units of length */
    dtheta = PI/n;  /* length of polygon along curvature */

    /* for each layer of polygons along */
    /* length of cylinder ... */
    for (x = -1.5; x < 1.5; x = x+dx) {
	/* ... and for each polygon describing */
	/* the circumference */
        for (theta = 0.0; theta < TWOPI; theta += dtheta) {	

	    /* calculate the four points */
	    /* describing the polygon */
	    p->vertex[0].x =
		p->vertex[1].x = x;
	    p->vertex[0].y = p->vertex[3].y = 
		RADIUS * cos(theta);
	    p->vertex[0].z = p->vertex[3].z = 
		RADIUS * sin(theta);
	    p->vertex[1].y = p->vertex[2].y = 
		RADIUS * cos(theta + dtheta);
	    p->vertex[1].z = p->vertex[2].z = 
		RADIUS * sin(theta + dtheta);
	    p->vertex[2].x = p->vertex[3].x = x + dx;

	    /* calculate the four normals of unit length */
	    for (vertex_i = 0; vertex_i < 4; vertex_i++) {
		p->normal[vertex_i].x = 0;
		p->normal[vertex_i].y = 
		    p->vertex[vertex_i].y / RADIUS;
		p->normal[vertex_i].z = 
		    p->vertex[vertex_i].z / RADIUS;
	    }
	    p++; 

	}
    }
}

/* 
** draw_cylinder()
** This subroutine increments through the 4
** vertices describing each polygon of
** the cylinder defined in make_cylinder.
** Note how a normal is sent down the
** graphics pipeline before each vertex
** so that the Graphics Library will
** compute the color for each vertex
** based on the lighting parameters that we
** are using.
*/
draw_cylinder()
{
    POLYGON *p;	/* pointer into polygon list */
    int poly_i;	/* polygon counter */

    /* start at first polygon and */
    /* increment through all of them */
    p = polygon;
    for (poly_i = 0; poly_i < number_of_polys; poly_i++) {
	bgnpolygon();	/* describe the polygon */
            n3f(&p->normal[0]);  
            v3f(&p->vertex[0]);  
            n3f(&p->normal[1]);  
            v3f(&p->vertex[1]);  
            n3f(&p->normal[2]);  
            v3f(&p->vertex[2]);  
            n3f(&p->normal[3]);  
            v3f(&p->vertex[3]);  
	endpolygon();
	p++;		/* go to the next polygon */
    }
}


/*
** main()
*/
main()
{
    int i = 0;

    if(!getgdesc(GD_LIGHTING_TWOSIDE)) { /* test for machine capability */
       printf("You do not have two sided light capabilities");
       exit(0);
    }
    /* set up graphics environment */
    prefposition(100, 600, 100, 600);
    winopen("cylinder");
    RGBmode();
    doublebuffer();
    gconfig();
    lsetdepth(0, 0x7FFFFF);
    zbuffer(TRUE);
    subpixel(TRUE);

    /* Use mmode() to set up projection */
    /* and viewing matrices for lighting */
    mmode(MVIEWING);		
    perspective(400, 1.0, 4.0, 12.0);
    loadmatrix(idmat);
    lookat(0.0,0.0,8.0,0.0,0.0,0.0,0);

    /* let there be light !!!! */
    def_simple_light_calc();
    use_simple_light_calc();

    /* Rotate cylinder in 2 deg. increments */
    /* about Y and Z axis for 180 frames */
    make_cylinder(25);
    while(TRUE){
	lmbind(BACKMATERIAL, getbutton(LEFTMOUSE) ? 0 : 2);
        c3f(blackvec);	/* clear the frame */
        clear();
	zclear();
	pushmatrix();		/* make a frame */
	    rot(i * 2.0, 'Z');
	    rot(i++ * 2.0, 'Y'); 
            draw_cylinder();
	popmatrix();
	swapbuffers();
    }
}
