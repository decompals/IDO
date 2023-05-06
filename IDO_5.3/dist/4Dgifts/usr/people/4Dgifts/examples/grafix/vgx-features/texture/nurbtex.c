/* 		nurbtex.c
 *
 *Description: This is nearly identical to 4Dgifts/examples/nurbs/nurbs.c
 *with a few enhancements. This demenstrates in addition to nurbs, how to
 *add texture to a nurbs surface. The graphics library allows multiple
 *calls to "nurbssurface" between a bgn/end surface pair. One such surface
 *must have positional data. To add texturing, make another call to nurbsur-
 *face specifying type as texture. Additionally , this porgram allows
 *the user to select display porperty through the use of popup menus. Also
 *shown is the mixing piecewise linear trim curves with nurbs trim curves.
 *Press the LEFTMOUSE button to see this.
 *
 *Modified by Martin R. McDoanld
 *            SGI
 *	      JULY 1990
 *
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

#define NUMCOORDS	3   
#define ORDER		4  

/* number of control points in each dimension of NURBS */
#define S_NUMPOINTS	(S_NUMKNOTS - S_ORDER)
#define T_NUMPOINTS	(T_NUMKNOTS - T_ORDER)

/* trimming */
int trim_flag = 0;

long zfar;

double	surfknots[S_NUMKNOTS] = {
	-1., -1., -1., -1., 1., 1., 1., 1. 
	};

double ctlpoints[S_NUMPOINTS*S_NUMPOINTS * S_NUMCOORDS] = {
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
/*  order + number of points = # of knots in knotvector */

double  trknots[8] = {
	0., 0., 0., 1.,  2., 2.5,  3., 4.,
	};

/*  repeating end points order -1 one times guarantees that the
    nurbscurve will pass through the endpoints. (this is true for
    all control points, but to close this nubrscurve with a pwlcurve
    we need to have the curve pass through the endpoints.)
*/

double trimpts[15] = {
    1.0, 0.0, 1.0,
    1.0, 0.0, 1.0,
    0.0, 2.0, 2.0,
    -1.0, 0.0, 1.0,
    -1.0, 0.0, 1.0,
    };

/* the begin point for this pwlcurve coincides with the end point of
   the nurbscurve. end point of the pwl curve coincides with the
   bgnpoint of the nurbscurve. this must happen or error will occur.
*/

double  pwlclose[4] = {
	  -1.0, 0.,
	  1.0, 0.,
	  };

double  pwlpoints[10] = {
	  0.25,0.25,
	  0.25,0.1,
         -0.25,0.1,
         -0.25,0.25,
	  0.25,0.25,
	  };

double texknots[4] = { -1., -1., 1., 1. };
double tclpoints[4][2] = { 0., 0.,  1., 0., 0.,1., 1., 1. };

float   idmat[4][4] = {
	1.0, 0.0, 0.0, 0.0,
	0.0, 1.0, 0.0, 0.0,
	0.0, 0.0, 1.0, 0.0,
	0.0, 0.0, 0.0, 1.0
	};


float texps[] = {TX_MINFILTER,TX_MIPMAP_BILINEAR,TX_MAGFILTER,TX_BILINEAR,0};
float tevps[] = {0};
float params[4] = { 0., 1., 0., .0};
float params2[4] = { 1., 0., 0., .0};
unsigned long  *image;

main()
{
    long dev;
    short val;
    int  menu, pupval;

    init_windows();
    setup_queue();
    init_view(); 
    make_lights();

 menu = defpup("PROPERTIES : %t|FILLED|OUTLINED POLYS|OUTLINE PATCH %x5|ISOLI NE_S %x12|EXIT%x6");

    set_scene();
    setnurbsproperty( N_ERRORCHECKING, 1.0 );
    setnurbsproperty( N_PIXEL_TOLERANCE, 50.0 );
    draw_trim_surface();

    while(TRUE) {
	while(qtest()) {
	    dev=qread(&val);
	    switch(dev) {
	    case RIGHTMOUSE:
		pupval = dopup(menu);
		    if (pupval == 6){
			  gexit();
			exit(0);
		    }
		    setnurbsproperty(N_DISPLAY,pupval);
	            break;
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
#ifdef  DISPLAY_LIST_VERSION
    char *prog = "dlnurbs";
#else
    char *prog = "nurbs";
#endif

    if (getgdesc(GD_BITS_NORM_DBL_RED) <= 0) {
        fprintf(stderr, "%s: requires double buffered RGB which is "
                        "unavailable on this machine\n", prog);
        exit(1);
        /* NOTREACHED */
    }
    if(!getgdesc(GD_TEXTURE)){
        printf("This only works on VGX machines/n");
        exit(2);
    }
    winopen("texture nurbs");
    wintitle("NURBS Surface");
    doublebuffer();
    RGBmode();
    gconfig();
    zbuffer( TRUE );
    subpixel( TRUE );
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

    inittext();

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
	/* This nurbssurface is positional data. */
	nurbssurface( 
	    sizeof( surfknots) / sizeof( double ), surfknots, 
	    sizeof( surfknots) / sizeof( double ), surfknots, 
	    sizeof(double) * S_NUMCOORDS, 
	    sizeof(double) * S_NUMPOINTS * S_NUMCOORDS, 
	    ctlpoints, 
	    S_ORDER, S_ORDER, 
	    N_XYZ
	    );
	/* This nurbssurface is texture data. */
	nurbssurface(4, texknots, 4, texknots, sizeof(double)*2,
		    sizeof(double)*2*2, &tclpoints[0][0],
		    2,2, N_T2D);
	/* trimming */
	if( trim_flag ) {
	    bgntrim();
	    /* trim curve is a rational quadratic nurbscurve (circle) */
            /*nurbscurve( 
		sizeof( trimknots) / sizeof( double ),
		trimknots,
		sizeof(double) * T_NUMCOORDS,
		trimpoints,
		T_ORDER,
	        N_STW
		);*/
	    nurbscurve(sizeof(trknots) / sizeof(double),
		trknots,
		sizeof(double) * NUMCOORDS,
		trimpts,
		ORDER -1,
		N_STW
		);
		pwlcurve(2,pwlclose,sizeof(double)*2,N_ST);
	endtrim();
	bgntrim();
		 pwlcurve(5,pwlpoints, sizeof( double )*2, N_ST);
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
		DIFFUSE,  0.3, 0.3, 0.3,
		SPECULAR,  0.6, 0.6, 0.6,
		SHININESS, 2.0,
		LMNULL
        };
        lmdef(DEFMATERIAL, 1, sizeof(array), array);
    }

    /* turn on lighting */
    lmbind(LIGHT0, 1);
    lmbind(LMODEL, 1);
    lmbind(MATERIAL, 1); 
}


inittext()
{

    image=(unsigned long *)longimagedata("/usr/demos/data/textures/brick.rgb");
    texdef2d(1,4,128,128,image,1,texps);
    tevdef(1,1,tevps);
    texbind(TX_TEXTURE_0,1);
    tevbind(TV_ENV0,1);
}

