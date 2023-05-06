/*
 *      nurbs.c
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define S_NUMKNOTS	8   /* number of knots in each dimension of surface*/
#define S_NUMCOORDS	3   /* number of surface coordinates, (x,y,z) */
#define S_ORDER		4   /* surface is bicubic, order 4 for each parameter */
#define T_NUMKNOTS	12  /* number of knots in the trimming curve */
#define T_NUMCOORDS	3   /* number of curve coordinates, (wx,wy,w) */
#define T_ORDER		3   /* trimming curve is rational quadratic */

/* number of control points in each dimension of NURBS */
#define S_NUMPOINTS	(S_NUMKNOTS - S_ORDER)
#define T_NUMPOINTS	(T_NUMKNOTS - T_ORDER)

/* trimming */
int trim_flag = 0;

long zfar;

double	surfknots[S_NUMKNOTS] = {
	-1., -1., -1., -1., 1., 1., 1., 1. 
	};

double ctlpoints[S_NUMPOINTS][S_NUMPOINTS * S_NUMCOORDS] = {
	-2.5,  -3.7,  1.0,
	-1.5,  -3.7,  3.0,
 	1.5,  -3.7, -2.5,
 	2.5,  -3.7,  -.75,

	-2.5,  -2.0,  3.0,
	-1.5,  -2.0,  4.0,
 	1.5,  -2.0,  -3.0,
 	2.5,  -2.0,  0.0,

	-2.5, 2.0,  1.0,
	-1.5, 2.0,  0.0,
 	1.5,  2.0,  -1.0,
 	2.5,  2.0,  2.0,

	-2.5,  2.7,  1.25,
	-1.5,  2.7,  .1,
 	1.5,  2.7,  -.6,
 	2.5,  2.7,  .2
	};

double	trimknots[T_NUMKNOTS] = {
	0., 0., 0.,  1., 1.,  2., 2.,  3., 3.,   4., 4., 4.
	};

double trimpoints[T_NUMPOINTS][T_NUMCOORDS] = {
	1.0, 0.0, 1.0,
	1.0, 1.0, 1.0,
	0.0, 2.0, 2.0,
	-1.0, 1.0, 1.0,
	-1.0, 0.0, 1.0,
	-1.0, -1.0, 1.0,
	0.0, -2.0, 2.0,
	1.0, -1.0, 1.0, 
	1.0, 0.0, 1.0,
	};

float   idmat[4][4] = {
	1.0, 0.0, 0.0, 0.0,
	0.0, 1.0, 0.0, 0.0,
	0.0, 0.0, 1.0, 0.0,
	0.0, 0.0, 0.0, 1.0
	};

main()
{
    long dev;
    short val;

    init_windows();
    setup_queue();
    init_view(); 
    make_lights();

    set_scene();
    setnurbsproperty( N_ERRORCHECKING, 1.0 );
    setnurbsproperty( N_PIXEL_TOLERANCE, 50.0 );
    draw_trim_surface();

    while(TRUE) {
	while(qtest()) {
	    dev=qread(&val);
	    switch(dev) {
		case ESCKEY:
		    gexit(); 
		    exit(0); 
		    break;
		case WINQUIT:
		    gexit(); 
		    dglclose(-1); 	/* this for DGL only */ 
		    exit(0); 
		    break;
		case REDRAW:
		    reshapeviewport();
		    set_scene();
	    	    draw_trim_surface();
		    break;
		case LEFTMOUSE:
		    if( val ) 
			trim_flag = !trim_flag; /* trimming */
		    break;
		default:
		    break;
	    }
	}
	set_scene();
	draw_trim_surface();
    }
}

init_windows()
/*---------------------------------------------------------------------------
 * Initialize all windows
 *---------------------------------------------------------------------------
 */
{
    if (getgdesc(GD_BITS_NORM_DBL_RED) <= 0) {
        fprintf(stderr, "nurbs: requires double buffered RGB which is "
                        "unavailable on this machine\n");
        exit(1);
        /* NOTREACHED */
    }
    winopen("nurbs");
    wintitle("NURBS Surface");
    doublebuffer();
    RGBmode();
    gconfig();
    zbuffer( TRUE );
    glcompat(GLC_ZRANGEMAP, 0);
    zfar = getgdesc(GD_ZMAX);
}

setup_queue()
/*---------------------------------------------------------------------------
 * Queue all devices
 *---------------------------------------------------------------------------
 */
{
    qdevice(ESCKEY);
    qdevice(RIGHTMOUSE);
    qdevice(WINQUIT);
    qdevice(LEFTMOUSE);	/* trimming */
}

init_view()
/*---------------------------------------------------------------------------
 * Initialize view and lighting mode
 *---------------------------------------------------------------------------
 */
{
    mmode(MPROJECTION);
    ortho( -4., 4., -4., 4., -4., 4. ); 

    mmode(MVIEWING);
    loadmatrix(idmat);

}

set_scene()
/*---------------------------------------------------------------------------
 * Clear screen and rotate object
 *---------------------------------------------------------------------------
 */
{
    lmbind(MATERIAL, 0); 
    RGBcolor(150,150,150);
    lmbind(MATERIAL, 1); 

    czclear(0x00969696, zfar);

    rotate( 100, 'y' );
    rotate( 100, 'z' ); 
}

draw_trim_surface()
/*---------------------------------------------------------------------------
 * Draw NURBS surface
 *---------------------------------------------------------------------------
 */
{
    bgnsurface();
	nurbssurface( 
	    sizeof( surfknots) / sizeof( double ), surfknots, 
	    sizeof( surfknots) / sizeof( double ), surfknots, 
	    sizeof(double) * S_NUMCOORDS, 
	    sizeof(double) * S_NUMPOINTS * S_NUMCOORDS, 
	    (double *)ctlpoints, 
	    S_ORDER, S_ORDER, 
	    N_V3D
	    );
	/* trimming */
	if( trim_flag ) {
	    bgntrim();
	    /* trim curve is a rational quadratic nurbscurve (circle) */
            nurbscurve( 
		sizeof( trimknots) / sizeof( double ),
		trimknots,
		sizeof(double) * T_NUMCOORDS,
		(double *)trimpoints,
		T_ORDER,
	        N_P2DR
		);
	    endtrim();
	}
    endsurface();
    swapbuffers();
}

make_lights()
/*---------------------------------------------------------------------------
 * Define material, light, and model 
 *---------------------------------------------------------------------------
 */
{
    /* initialize model and light to default */
    lmdef(DEFLMODEL,1,0,0);
    lmdef(DEFLIGHT,1,0,0);

    /* define material #1 */
    {
        static float array[] = {
		EMISSION, 0.0, 0.0, 0.0,
		AMBIENT,  0.1, 0.1, 0.1,
		DIFFUSE,  0.6, 0.3, 0.3,
		SPECULAR,  0.0, 0.6, 0.0,
		SHININESS, 2.0,
		LMNULL
        };
        lmdef(DEFMATERIAL, 1, sizeof(array)/sizeof(array[0]), array);
    }

    /* turn on lighting */
    lmbind(LIGHT0, 1);
    lmbind(LMODEL, 1);
    lmbind(MATERIAL, 1); 
}
