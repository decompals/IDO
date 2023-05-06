/*
 *    heart.c
 *
 *   somebody "out there" might be wondering why you'd ever want to change 
 *   continuity conditions on a NuRBS, so in honor of Valentine's Day here
 *   is a NuRBS heart which has a non-uniform knot sequence.
 *
 *    This program demonstrates an example of using both a non-uniform 
 *    knot sequence and the rational component of NURBS.
 *
 *                                   Jill Huchital - February 1991
*/
#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <math.h>

/* number of points, number of knots, and order in the s and t directions */
#define S_NUMPOINTS	13
#define S_ORDER		3   /* quadratic, degree 2 */
#define S_NUMKNOTS	(S_NUMPOINTS + S_ORDER)

#define T_NUMPOINTS 3
#define T_ORDER		3 
#define T_NUMKNOTS	(T_NUMPOINTS + T_ORDER)

#define SQRT_TWO	1.414

typedef double Point[4];

void main(void);
void draw_nurb(void);
void initialize(void);

long zfar;

double	sknots[S_NUMKNOTS];
double	tknots[T_NUMKNOTS];

/* Knot sequence.  The multiple knot at value 4 makes the NURBS go through
 * the point between the two bumps of the heart.
 */
static double sknots[S_NUMKNOTS] = {-1.0, -1.0, -1.0, 0.0, 1.0, 2.0, 3.0, 4.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 9.0, 9.0};

/* Knot sequence in t.*/
static double tknots[T_NUMKNOTS] = {1.0, 1.0, 1.0, 2.0, 2.0, 2.0};

/* control points go {wx, wy, wz, w}.  Note that for a given
 * s value, the control points are colinear.  This makes the NuRBS
 * flat in T, so it looks like a ribbon arranged in a heart shape. 
 */
Point ctlpoints[S_NUMPOINTS][T_NUMPOINTS] = 
{
	{	{4.,2.,2.,1.},{4.,1.6,2.5,1.},{4.,2.,3.0,1.}	},
	{	{5.,4.,2.,1.},{5.,4.,2.5,1.},{5.,4.,3.0,1.}	},
	{	{6.,5.,2.,1.},{6.,5.,2.5,1.},{6.,5.,3.0,1.}	},
	{	{SQRT_TWO*6.,SQRT_TWO*6.,SQRT_TWO*2.,SQRT_TWO},{SQRT_TWO*6.,SQRT_TWO*6.,SQRT_TWO*2.5,SQRT_TWO},{SQRT_TWO*6.,SQRT_TWO*6.,SQRT_TWO*3.0,SQRT_TWO}	},
	{	{5.2,6.7,2.,1.},{5.2,6.7,2.5,1.},{5.2,6.7,3.0,1.}	},
	{	{SQRT_TWO*4.,SQRT_TWO*6.,SQRT_TWO*2.,SQRT_TWO},{SQRT_TWO*4.,SQRT_TWO*6.,SQRT_TWO*2.5,SQRT_TWO},{SQRT_TWO*4.,SQRT_TWO*6.,SQRT_TWO*3.0,SQRT_TWO}	},
	{	{4.,5.2,2.,1.},{4.,4.6,2.5,1.},{4.,5.2,3.0,1.}	},
	{	{SQRT_TWO*4.,SQRT_TWO*6.,SQRT_TWO*2.,SQRT_TWO},{SQRT_TWO*4.,SQRT_TWO*6.,SQRT_TWO*2.5,SQRT_TWO},{SQRT_TWO*4.,SQRT_TWO*6.,SQRT_TWO*3.0,SQRT_TWO}	},
	{	{2.8,6.7,2.,1.},{2.8,6.7,2.5,1.},{2.8,6.7,3.0,1.}	},
	{	{SQRT_TWO*2.,SQRT_TWO*6.,SQRT_TWO*2.,SQRT_TWO},{SQRT_TWO*2.,SQRT_TWO*6.,SQRT_TWO*2.5,SQRT_TWO},{SQRT_TWO*2.,SQRT_TWO*6.,SQRT_TWO*3.0,SQRT_TWO}	},
	{	{2.,5.,2.,1.},{2.,5.,2.5,1.},{2.,5.,3.0,1.}	},
	{	{3.,4.,2.,1.},{3.,4.,2.5,1.},{3.,4.,3.0,1.}	},
	{	{4.,2.,2.,1.},{4.,1.6,2.5,1.},{4.,2.,3.0,1.}	}
};

Boolean rotating = FALSE;

long rotx = 40, roty = 40;

void
main(void)
{
    long dev;
    short val;
    long x_half, y_half;

    initialize();

    x_half = getgdesc(GD_XPMAX)/2;
    y_half = getgdesc(GD_YPMAX)/2;

    while(TRUE) {

		while(qtest()) {

		    dev=qread(&val);
		    switch(dev) {

				case ESCKEY:
					if (val)
				    	exit(0); 
				    break;

				case MIDDLEMOUSE:
					if (val )
						rotating = TRUE;
					else
						rotating = FALSE;
					break;
				case REDRAW:
				    reshapeviewport();
				    break;
	
				case INPUTCHANGE:
					break;

			}
	        }

		if (rotating) {
			rotx = getvaluator(MOUSEX) - x_half;
			roty = getvaluator(MOUSEY) - y_half;
		}

		draw_nurb();
	}
}

	
void initialize(void)
{
    long gid;

    prefposition(500,900,300,700);
    gid = winopen("NURBS Surface");
    keepaspect(1,1);
    winconstraints();
    doublebuffer();
    RGBmode();
    gconfig();
    zbuffer( TRUE );
    zfar = getgdesc(GD_ZMAX);

    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    qdevice(RIGHTMOUSE);

    qenter(REDRAW,gid);
	
    mmode(MVIEWING);
	
    window(-2.0, 2.0, -3.0, 3.0, 1.0, 10.0);
    lookat(4.0, 4.5, 5.0, 4.0, 4.5, 0.0, 0);

    {
	static float lt_array[] = {
		POSITION, 0.0, 1.0, 1.0, 0.0,
		LCOLOR, 1.0, 0.8, 0.0, 
		LMNULL
		};
	lmdef(DEFLIGHT,1,sizeof(lt_array), lt_array);
    }
    {
	static float lt2_array[] = {
		POSITION, 15.0, 10.0, 1.0, 1.0,
		LCOLOR, 0.5, 0.0, 1.0,
		LMNULL
		};
	lmdef(DEFLIGHT,2,sizeof(lt2_array), lt2_array);
    }
    {
	static float lt3_array[] = {
		POSITION, -1.5, 1.0, 1.0, 1.0,
		LCOLOR, 1.0, 0.0, 0.0,
		LMNULL
		};
	lmdef(DEFLIGHT,3,sizeof(lt3_array), lt3_array);
    }
    {
        static float array[] = {
		EMISSION, 0.0, 0.0, 0.0,
		AMBIENT,  0.1, 0.0, 0.0,
		DIFFUSE,  0.8, 0.8, 0.8,
		SPECULAR,  0.4, 0.4, 0.4,
		SHININESS, 25.0,
		LMNULL
        };
	lmdef(DEFMATERIAL, 1, sizeof(array), array);
    }

    if (getgdesc(GD_LIGHTING_TWOSIDE)) {
	{
            static float bk_array[] = {
			EMISSION, 0.0, 0.0, 0.0,
			AMBIENT,  0.1, 0.0, 0.0,
			DIFFUSE,  0.7, 0.7, 0.7,
			SPECULAR,  0.6, 0.6, 0.6,
			SHININESS, 10.0,
			LMNULL
            };
            lmdef(DEFMATERIAL, 2, sizeof(bk_array), bk_array);
        }
	lmbind(BACKMATERIAL, 1);
	{
	    static float lm_array[] = {
			TWOSIDE, 1.0,
			LMNULL
	    };
	    lmdef(DEFLMODEL, 1, 3, lm_array);
	}

    } else {

    	lmdef(DEFLMODEL,1,0,0);
    }
    lmbind(LMODEL, 1);
    lmbind(LIGHT0, 1);
    lmbind(LIGHT2, 2);
    lmbind(LIGHT3, 3);
    lmbind(MATERIAL,1);
	
    setnurbsproperty( N_ERRORCHECKING, 1.0 );
    setnurbsproperty( N_PIXEL_TOLERANCE, 25.0 );
}


void draw_nurb(void)
{
    czclear(0x969696, zfar);

	pushmatrix();
		translate(4.,4.5,0.);
		rotate(roty, 'x');
		rotate(rotx, 'y');
		translate(-4.,-4.5,0.);

		bgnsurface();
			nurbssurface( 
				S_NUMKNOTS, sknots,
				T_NUMKNOTS, tknots,
				sizeof(Point) * T_NUMPOINTS,
				sizeof(Point),
				ctlpoints, 
				T_ORDER, S_ORDER,
				N_V3DR);
		endsurface();
		
	popmatrix();
	
    swapbuffers();
}
