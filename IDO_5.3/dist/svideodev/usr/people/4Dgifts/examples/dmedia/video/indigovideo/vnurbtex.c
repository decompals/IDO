/* vnurbtex.c
 * 
 * This program demonstrates how to texture map live video onto
 * the surface of a non-uniform rational b-spline (nurbs). it
 * supports two modes of video capture: 32 bit rgb and 8 bit
 * gray scale. The user can toggle between these two modes by
 * pressing the right mouse button. This code was based on
 * nurbtex (4Dgifts/examples/vgx/texture/nurbtex.c) which in turn
 * was based on nurbs (4Dgifts/examples/nurbs/nurbs.c)
 * 
 * modified by chuck tuffli
 */

#ident "$Revision: 1.1 $"

#include <stdio.h>
#include <stdlib.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <gl/get.h>
#include <svideo.h>

#define S_NUMKNOTS	8   /* number of knots in each dimension of surface*/
#define S_NUMCOORDS	3   /* number of surface coordinates, (x,y,z) */
#define S_ORDER		4   /* surface is bicubic, order 4 for each parameter */

/* number of control points in each dimension of NURBS */
#define S_NUMPOINTS	(S_NUMKNOTS - S_ORDER)

#define RGB_COMP	4   /* select a 4 component texture */
#define MONO_COMP	1   /* select a 1 component texture */

#define GRAY_LEVELS	16  /* 8 bit video double buffered = 4 bits
			     * or 16 levels of gray
			     */
/* data for the nurb */
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

/* texture info for the nurb surface */
double texknots[4] = { -1., -1., 1., 1. };
double tclpoints[4][2] = { 0., 0.,  1., 0., 0.,1., 1., 1. };

/* identity matrix for lighting in color mode */
float   idmat[4][4] = {
	1.0, 0.0, 0.0, 0.0,
	0.0, 1.0, 0.0, 0.0,
	0.0, 0.0, 1.0, 0.0,
	0.0, 0.0, 0.0, 1.0
	};

/*
 * texture function description.
 * the bilinear filter is slower than
 * the point filter,  but improves the
 * image quality significantly
 * 
 */
float	texps[] = {TX_MINFILTER, TX_BILINEAR,
		    TX_MAGFILTER, TX_BILINEAR,
		    TX_NULL};

/* texture environment definition */ 
float	tevps[] = {TV_NULL};

unsigned long *image, *imageInter;
int	    xsize, ysize;
SVhandle    V;
int	    captureFormat;
long	    vParams[2];
short	    oldCMap[GRAY_LEVELS][3];

static void initGrayScale(short [GRAY_LEVELS][3]);
static void restoreColorMap(short [GRAY_LEVELS][3]);

main()
{
    long    dev;
    short   val;
    int	    notDone = 1;
    
    init_windows();
    init_video();
    setup_queue();
    init_view(); 
    make_lights();

    setnurbsproperty( N_ERRORCHECKING, 0.0 );
    setnurbsproperty( N_PIXEL_TOLERANCE, 50.0 );
    
    mySetTexture();
    first_rot();

    while(notDone) {
	mySetTexture();
	draw_surface();
	
	while(qtest()) {
	    dev=qread(&val);
	    qreset();
	    switch(dev) {
		    case ESCKEY:
			notDone = 0; 
			break;
		    case WINQUIT:
		    case WINSHUT:
			dglclose(-1); 	/* this for DGL only */
			notDone = 0; 
			break;
		    case RIGHTMOUSE:
			if (val)
			    switch_mode();
			break;
		    case REDRAW:
			reshapeviewport();
			draw_surface();
			break;
		    default:
			break;
	    }
	}
    }
    
    /* 
     * clean up after ourselves before leaving.
     * reset the video and then the color map
     * 
     */
    vParams[0] = SV_COLOR;
    vParams[1] = SV_DEFAULT_COLOR;
    if (svSetParam(V, vParams, 2) < 0) {
	svPerror("svSetParam");
    }
    svCloseVideo(V);
    
    restoreColorMap(oldCMap);
    gconfig();
    
    gexit();
}

/*
 * Initialize all windows
 *
 */
init_windows()

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
        printf("This only works on machines with texture maping\n");
        exit(2);
    }
    
    keepaspect(1, 1);
    winopen("texture nurbs");
    wintitle("NURBS Surface");
    dither(DT_ON);
    doublebuffer();
    RGBmode();
    zbuffer( TRUE );
    subpixel( TRUE );
    glcompat(GLC_ZRANGEMAP, 0);
    zfar = getgdesc(GD_ZMAX);
    gconfig();
}

/*
 * Queue all devices
 *
 */
setup_queue()

{
    qdevice(ESCKEY);
    qdevice(WINQUIT);
    qdevice(WINSHUT);
    qdevice(RIGHTMOUSE);    /* switch between color and gray scale */
}

/*
 * initialize view and lighting mode
 * then initialize the gray scale and
 * texture
 *
 */
init_view()

{
    mmode(MPROJECTION);
    ortho( -4., 4., -4., 4., -4., 4. ); 

    mmode(MVIEWING);
    loadmatrix(idmat);

    initGrayScale(oldCMap);
    
    mySetTexture();
}

/*
 * give our nurbs an interesting
 * schpin
 * 
 */
first_rot()

{
    rotate( 300, 'y' );
    rotate( 300, 'x' );
}

/*
 * Clear screen and rotate object
 * then draw NURBS surface
 *
 */
draw_surface()

{
    czclear(0x0, zfar);
    rotate( 100, 'z' );

    /*
     * make this call to correctly modulate
     * the 8 bit component(s) to their 4 bit
     * texture values
     * 
     */
    cpack(0xff0f0f0f);
    bgnsurface();
	/* This nurbssurface is positional data. */
	nurbssurface( 
	    sizeof( surfknots) / sizeof( double ), surfknots, 
	    sizeof( surfknots) / sizeof( double ), surfknots, 
	    sizeof(double) * S_NUMCOORDS, 
	    sizeof(double) * S_NUMPOINTS * S_NUMCOORDS, 
	    (double *)ctlpoints, 
	    S_ORDER, S_ORDER, 
	    N_XYZ
	    );
	/* This nurbssurface is texture data. */
	nurbssurface(4, texknots, 4, texknots, sizeof(double)*2,
		    sizeof(double)*2*2, &tclpoints[0][0],
		    2,2, N_T2D);
    endsurface();
    swapbuffers();
}

/*
 * Define material, light, and model 
 *
 */
make_lights()

{
    /* initialize model and light to default */
    lmdef(DEFLMODEL,1,0,0);
    lmdef(DEFLIGHT,1,0,0);

    /* define material #1 */
    {
        static float lightDefs[] = {
		EMISSION, 0.0, 0.0, 0.0,
		AMBIENT,  0.1, 0.1, 0.1,
		DIFFUSE,  0.3, 0.3, 0.3,
		SPECULAR,  0.6, 0.6, 0.6,
		SHININESS, 2.0,
		LMNULL
        };
        lmdef(DEFMATERIAL, 1, sizeof(lightDefs), lightDefs);
    }

    /* turn on lighting */
    lmbind(LIGHT0, 1);
    lmbind(LMODEL, 1);
    lmbind(MATERIAL, 1); 
}

/*
 * this actually dma's video from
 * the video board in either 32 bit
 * rgb or 8 bit gray scale and uses
 * the result as the new texture for
 * the nurb
 * note that the 8 bit gray scale
 * is returned to the user as two
 * fields which then must be interleaved
 * 
 */
mySetTexture()

{
    svCaptureOneFrame(V, captureFormat, &xsize, &ysize, (void *)image);
    
    if (captureFormat == SV_RGB8_FRAMES) {
	svInterleaveFields(TRUE, (char *)image, (char *)imageInter, xsize, ysize);
	texdef2d(1,MONO_COMP,xsize,ysize,imageInter,1, texps);
    } else
	texdef2d(1,RGB_COMP,xsize,ysize,image,1, texps);

    tevdef(1,1,tevps);
    texbind(TX_TEXTURE_0,1);
    tevbind(TV_ENV0,1);
}

/*
 * set up video to capture single frames
 * 
 */
init_video()

{
    xsize = 128;
    ysize = 96;

    image = malloc(xsize*ysize*sizeof(long));
    imageInter = malloc(xsize*ysize*sizeof(long));
    if ((V = svOpenVideo()) == NULL) {
	fprintf(stderr, "Unable to access the video board\n");
	exit(1);
    }

    captureFormat = SV_RGB32_FRAMES;	/* default */
}

/*
 * switch between RGBmode and
 * color index mode (cmode)
 * note that the lighting must be disabled
 * before going into cmode() and enabled
 * for RGBmode() to work
 * 
 */
switch_mode()

{
    switch(getdisplaymode()) {
	case DMDOUBLE:
	    vParams[0] = SV_COLOR;
	    vParams[1] = SV_DEFAULT_COLOR;
	    if (svSetParam(V, vParams, 2) < 0) {
		svPerror("svSetParam");
	    }
	    captureFormat = SV_RGB32_FRAMES;
	    RGBmode();
	    gconfig();
	    lmbind(MATERIAL, 1);
	    break;
	case DMRGBDOUBLE:
	    vParams[0] = SV_COLOR;
	    vParams[1] = SV_MONO;
	    if (svSetParam(V, vParams, 2) < 0) {
		svPerror("svSetParam");
	    }
	    captureFormat = SV_RGB8_FRAMES;
	    cmode();
	    gconfig();
	    lmbind(MATERIAL, 0);
	    break;
	default:
	    break;
    }
}

/*
 * set up our own gray scale color map and
 * preserve the old values so we can clean
 * up at the end
 * 
 */
static void
initGrayScale(short oldVals[GRAY_LEVELS][3])

{
    short   color;
    int	    i;
    
    for(i=0; i<GRAY_LEVELS; i++) {
	color = 255/(GRAY_LEVELS-1)*i;
	getmcolor(i, &(oldVals[i][0]), &(oldVals[i][1]), &(oldVals[i][2]));
	mapcolor(i, color, color, color);
    }
}

/*
 * restore color map to
 * its original condition
 * 
 */
static void
restoreColorMap(short oldVals[GRAY_LEVELS][3])

{
    int	    i;
    
    for(i=0; i<GRAY_LEVELS; i++)
	mapcolor(i, oldVals[i][0], oldVals[i][1], oldVals[i][2]);
}
