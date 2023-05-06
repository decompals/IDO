/*
 *  cylinder2.c:
 *
 *    Second example program from Chapter 9 on "Lighting" from the GT Graphics
 *  Library User's Manual.  This program displays two intersecting cylinders,
 *  using a different surface material for each cylinder.  In addition, each
 *  cylinder is lit with two light sources.
 *
 */
#include <gl.h>
#include <math.h>
#include <stdio.h>

#define RADIUS 0.9
#define TWOPI 6.28318530
#define PI 3.14159265

/* define black RGB color */
float  blackvec[3] = {0.0, 0.0, 0.0};	
Matrix idmat = {1.0,0.0,0.0,0.0,  /* identity matrix */
                0.0,1.0,0.0,0.0,
		0.0,0.0,1.0,0.0,
		0.0,0.0,0.0,1.0};

/* define a polygon with some structures */
typedef struct {	/* 3-D vertex structure */
    Coord x;
    Coord y;
    Coord z;
} POINT;

typedef struct {	/* lighted polygon struct */
    POINT vertex[4];
    POINT normal[4];
} POLYGON;

int number_of_polys;	/* cylinder polygon count */
POLYGON *polygon;	/* polygon list pointer */


/* define property arrays */
float shiny_material[] =  
    {SPECULAR,  0.8, 0.8, 0.8, /* light gray reflectance */
     DIFFUSE,   0.4, 0.4, 0.4, /* gray reflectance */
     SHININESS, 30.0,          /* focused highlight */
     LMNULL};

float purple_material[] = 
    {SPECULAR,  0.3, 0.3, 0.3, /* gray reflectance */
     DIFFUSE,   0.8, 0.0, 0.8, /* purple reflectance */
     SHININESS, 3.0,           /* unfocused highlight */
     AMBIENT,   0.2,0.0,0.2,   /* purple reflectance */
     LMNULL};

float blue_light[] =
    {LCOLOR,    0.0,0.0,0.6,   /* blue light */
     POSITION,  0.0,0.1,0.0,0.0,  /* Y axis at infinity */
     LMNULL};


/*
** def_light_calc()
** Tell the Graphics Library to DEFINE a
** lighting calculation that accounts for
** ambient, diffuse, and specular reflection.
** This lighting calculation defines a second
** material and light source.
*/ 
def_light_calc() { 
    lmdef(DEFMATERIAL, 1, 11, shiny_material); 
    lmdef(DEFMATERIAL, 2, 15, purple_material);
    lmdef(DEFLIGHT, 1, 0, NULL);
    lmdef(DEFLIGHT, 2, 10, blue_light);
    lmdef(DEFLMODEL, 1, 0, NULL);
}

/*
** use_light_calc()
** Tell the Graphics Library to USE
** the lighting calculation that we 
** defined earlier.
*/
use_light_calc()
{
    lmbind(LIGHT0, 1);	/* use light source description 1 */
    lmbind(LIGHT1, 2);	/* use light source description 2 */
    lmbind(LMODEL, 1);	/* use lighting model 1 */
}

/* 
** make_cylinder()
** Draw a cylinder using (2 * n) polygons
** to approximate the curvature and n polygons
** to describe the length. This requires (2 * n^2)
** polygons to describe the cylinder. Compute
** the surface normal at each vertex so we can
** use the hardware lighting facility to perform
** lighting calculations.
*/
make_cylinder(n)
int n;
{
    POLYGON *p;		/* polygon list pointer */
    float theta, dtheta,/* current angle and angle */
		        /* increment around section */
	  x, dx;	/* current position and */
			/* increment along cylinder side */
    int vertex_i;	/* vertex counter */

    /* allocate and point to enough */
    /* memory for all the polygons */
    number_of_polys = 2 * n * n;
    p = polygon = (POLYGON *)
	malloc(number_of_polys * sizeof(POLYGON));

    dx = 3.0/n;	/* n polygons for 3.0 units of length */
    dtheta = PI/n; /* length of polygon along curvature */

    /* for each layer of polygons */
    /* along length of cylinder ... */
    for (x = -1.5; x < 1.5; x = x+dx) {
	/* ... and for each polygon */
	/* describing the circumference */
        for (theta = 0.0; theta < TWOPI; theta += dtheta) {	
	    /* calculate the four points */
	    /* describing the polygon */
	    p->vertex[0].x = p->vertex[1].x = x;
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
** vertices describing each polygon of the
** cylinder defined in make_cylinder.  Note
** how a normal is sent to the graphics
** hardware before each vertex so that the
** lighting facility will compute the color
** for each vertex based on the lighting
** parameters that we are using.
*/
draw_cylinder()
{
    POLYGON *p;	 /* pointer into polygon list */
    int poly_i;	 /* polygon counter */

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
** Main Program 
*/
main()
{
    int i;

    /* set up graphics environment */
    prefposition(100, 600, 100, 600);
    winopen("cylinder");
    RGBmode();
    doublebuffer();
    gconfig();
    zbuffer(TRUE);

    /* Use mmode() to set up projection */
    /* and viewing matrices for lighting */
    mmode(MVIEWING);		
    perspective(400, 1.0, 4.0, 12.0);
    loadmatrix(idmat);
    lookat(0.0,0.0,8.0,0.0,0.0,0.0,0);

    /* let there be light !!!! */
    def_light_calc();
    use_light_calc();

    /* Rotate cylinders in 2 deg. increments */
    /* about Y and Z axis for 180 frames */
    make_cylinder(25);
    for (i = 0; i < 180; i++) {   
        c3f(blackvec);		  
        clear();
	zclear();

	pushmatrix();
    	    rot(i * 2.0, 'Z');
	    rot(i * 2.0, 'Y'); 
	    /* use white shiny material for cyl 1*/
	    lmbind(MATERIAL, 1);
            draw_cylinder();
	    pushmatrix();		  
	        rot(90.0, 'Y');
	        scale(0.9,0.9,0.9);
		/* use purple rough material for cyl 2 */
	        lmbind(MATERIAL, 2);
                draw_cylinder();
	    popmatrix();
	popmatrix();
	swapbuffers();
    }
    sleep(3);
}
