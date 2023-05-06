/*
 *    sgiflag.c:
 *
 *  This program displays a waving flag with an SGI logo trimmed out of
 *  it.  The flag is a single nurbs surface (bicubic, bezier). It "waves" 
 *  by making it control point oscillate on a sine wave.
 *
 *  The logo is cut from the flag using a combination of piecewise-linear 
 *  and bezier trim curves.
 *
 *                                    Howard Look - December 1990
 */

#include <stdio.h>
#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>
#include "sgiflag.h"
#include "logopoints.h"

long zfar;

/* Knot sequences for cubic bezier surface and trims */
Knot sknots[S_NUMKNOTS] = {0., 0., 0., 0., 1., 1., 1., 1.};
Knot tknots[T_NUMKNOTS] = {0., 0., 0., 0., 1., 1., 1., 1.};
Knot trimknots[S_NUMKNOTS] = {0., 0., 0., 0., 1., 1., 1., 1.};

/* Control points for the flag. The Z values are modified to make it wave */
Point ctlpoints[S_NUMPOINTS][T_NUMPOINTS] = {
    { {0., 3., 0.}, {1., 3., 0.}, {2., 3., 0}, {3., 3., 0.}},
    { {0., 2., 0.}, {1., 2., 0.}, {2., 2., 0}, {3., 2., 0.}},
    { {0., 1., 0.}, {1., 1., 0.}, {2., 1., 0}, {3., 1., 0.}},
    { {0., 0., 0.}, {1., 0., 0.}, {2., 0., 0}, {3., 0., 0.}}
};

/* Trim the whole exterior of the, counter clockwise. Necessary to do
 * internal trimming
 */
TrimCurve *whole;
TrimCurve ccl_path;
TrimPiece ccl_square[] = {
    {
        PWL,
        11,
        {
			{0., 0.}, {.25, 0.}, {.5, 0.}, {.75, 0.}, {1., 0.},
			{1., 1.}, {.75, 1.}, {.5, 1.}, {.25, 1.}, {0., 1.}, {0., 0.}
		}
    }
};

/* Three paths for three parts of the logo */
TrimCurve *path[3];

/* Initial one-third of the logo, centered at origin */
TrimCurve initial_path;
TrimPiece initial_pieces[] = {
        {
            PWL, /* 0 */
            6,
            {{Ax,Ay},{Bx,By},{Cx,Cy},{Dx,Dy},{Ex,Ey},{0.,0.}}
        },
        {
            CURVE, /* 1 */
            4,
            {{0.,0.},{Fx,Fy},{Fx,Fy},{0.,0.}}
        },
        {
            PWL, /* 2 */
            2,
            {{Gx, Gy},{Gx,Gy}}
        },
        {
            CURVE, /* 3 */
            4,
            {{0.,0.},{Gx,Gy},{Gx,Gy},{0.,0.}}
        },
        {
            PWL, /* 4 */
            3,
            {{0., 0.},{Z,Z},{0.,0.}}
        },
        {
            CURVE, /* 5 */
            4,
            {{0.,0.},{-Gx,Gy},{-Gx,Gy},{0.,0.}}
        },
        {
            PWL, /* 6 */
            2,
            {{-Fx, Fy},{-Fx,Fy}}
        },
        {
            CURVE, /* 7 */
            4,
            {{0.,0.},{-Fx,Fy},{-Fx,Fy},{0.,0.}}
        },
        {
            PWL, /* 8 */
            6,
            {{0.,0.},{-Ex,Ey},{-Dx,Dy},{-Cx,Cy},{-Bx,By},{Ax,Ay}}
        }
};

/* Main loop. Initializes the window, then displays the nurb over and over
 * while processing events.
 */
main(){
    long dev;
    short val;
    Boolean trimming = TRUE, filled = TRUE;

    initialize();

    while(TRUE) {

        while(qtest()) {

            dev=qread(&val);
            switch(dev) {

                case ESCKEY:
                    exit(0); 
                    break;
                    
                case REDRAW:
                    reshapeviewport();
                    break;

                case LEFTMOUSE:
                    if (val)
                        trimming = !trimming;
                    break;

                case MIDDLEMOUSE:
                    if (val) {
                        filled = !filled;
                        if (filled)
                            setnurbsproperty( N_DISPLAY, N_FILL);
                        else
                            setnurbsproperty( N_DISPLAY, N_OUTLINE_POLY);
                    }
                    break;
            }
        }
        draw_nurb(trimming);
    }
}

/* Given endpoints of the line a and b, the distance d from point a,
 * returns a new point (in result) that is on that line.
 */
void interp(TrimPoint a, TrimPoint b, double d, TrimPoint result) {

    double l;

    l = sqrt((a[0] - b[0])*(a[0] - b[0]) + (a[1] - b[1])*(a[1] - b[1]));

    result[0] = a[0] + (b[0] - a[0])*d/l; 
    result[1] = a[1] + (b[1] - a[1])*d/l; 
    
}

/* Given two trim pieces, coerces the endpoint of the first and the
 * start point of the second to be indentical.
 *
 * The two trims must be of opposite types, PWL or CURVE.
 */
void join_trims(TrimPiece *trim1, TrimPiece *trim2, double radius) {

    int last;
    TrimPoint result;

    last = trim1->points - 1;
    
    if (trim1->type == PWL)
        interp(trim2->point[1], trim1->point[last - 1], radius, result);
    else /* trim1 is CURVE */
        interp(trim1->point[last-1], trim2->point[0], radius, result);

    trim1->point[last][0] = trim2->point[0][0] = result[0];
    trim1->point[last][1] = trim2->point[0][1] = result[1];
}    

/* Translates each point in the trim piece by tx and ty */
void translate_trim(TrimPiece *trim, double tx, double ty) {

    int i;

    for (i=0; i<trim->points; i++) {
        trim->point[i][0] += tx;
        trim->point[i][1] += ty;
    }
}        

/* Scales each point in the trim piece by sx and sy */
void scale_trim(TrimPiece *trim, double sx, double sy) {

    int i;

    for (i=0; i<trim->points; i++) {
        trim->point[i][0] *= sx;
        trim->point[i][1] *= sy;
    }
}        

/* Rotates each point in the trim piece by angle radians about the origin */
void rotate_trim(TrimPiece *trim, double angle) {

    int i;
    double s,c;
    TrimPoint t;

    s = sin(angle);
    c = cos(angle);

    for (i=0; i<trim->points; i++) {
        t[0] = trim->point[i][0];
        t[1] = trim->point[i][1];
        
        trim->point[i][0] = c*t[0] - s*t[1];
        trim->point[i][1] = s*t[0] + c*t[1];
    }
}        

/* Creates storage space for dst and copies the contents of src into dst */
void copy_path(TrimCurve *src, TrimCurve **dst) {

    int i,j;

    *dst = (TrimCurve *) malloc(sizeof(TrimCurve));
    (*dst)->pieces = src->pieces;
    (*dst)->trim = (TrimPiece *) malloc((src->pieces)*sizeof(TrimPiece));

    for(i=0; i < src->pieces; i++) {
        (*dst)->trim[i].type = src->trim[i].type;
        (*dst)->trim[i].points = src->trim[i].points;

        for (j=0; j < src->trim[i].points; j++) {
            (*dst)->trim[i].point[j][0] = src->trim[i].point[j][0];
            (*dst)->trim[i].point[j][1] = src->trim[i].point[j][1];
        }
    }
}    

/* Initializes the outer whole trim plus the three trimming paths 
 * required to trim the logo.
 */
void init_trims(void) {

    int i;

	/* whole outer path, counter clockwise, so NuRB is not trimmed */
    whole = &ccl_path;
    whole->pieces = 1;
    whole->trim = ccl_square;

	/* initial third of logo, centered at origin */
    path[0] = &initial_path;
    path[0]->pieces = ELEMENTS(initial_pieces);
    path[0]->trim = initial_pieces;
    for(i=0; i < path[0]->pieces - 1; i++)
        join_trims(&path[0]->trim[i], &path[0]->trim[i+1], LOGO_RADIUS);

	/* copy other to other two thirds */
    copy_path(path[0],&path[1]);
    copy_path(path[0],&path[2]);

	/* scale and translate first third */
    for (i=0; i<path[0]->pieces; i++) {
        scale_trim(&path[0]->trim[i],0.5,1.0);
        translate_trim(&path[0]->trim[i],0.5,0.52);
    }

	/* rotate, scale and translate second third */
    for (i=0; i<path[1]->pieces; i++) {        
        rotate_trim(&path[1]->trim[i],2.0*M_PI/3.0);
        scale_trim(&path[1]->trim[i],0.5,1.0);
        translate_trim(&path[1]->trim[i],0.49,0.5);
    }

	/* rotate, scale and translate last third */
    for (i=0; i<path[2]->pieces; i++) {        
        rotate_trim(&path[2]->trim[i],2.0*2.0*M_PI/3.0);
        scale_trim(&path[2]->trim[i],0.5,1.0);
        translate_trim(&path[2]->trim[i],0.51,0.5);
    }
}
    
/* Opens a square window, and initializes the window, interesting devices,
 * viewing volume, material, and lights.
 */
void initialize(void) {

    keepaspect(1,1);
    foreground();
    winopen("NURBS Surface");

    doublebuffer();
    RGBmode();
    gconfig();
    
    zbuffer( TRUE );
    zfar = getgdesc(GD_ZMAX);

    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    qdevice(RIGHTMOUSE);

    mmode(MVIEWING);
    perspective(600,1.0,1.0,10.0);
    translate(0., 0., -6.);
    
    lmdef(DEFLMODEL,1,0,0);
    lmdef(DEFLIGHT,1,0,0);
    {
        static float array[] = {
            EMISSION, 0.0, 0.0, 0.0,
            AMBIENT,  0.1, 0.1, 0.1,
            DIFFUSE,  0.8, 0.1, 0.8,
            SPECULAR,  0.6, 0.6, 0.6,
            SHININESS, 2.0,
            LMNULL
        };
        lmdef(DEFMATERIAL, 1, sizeof(array), array);
    }
    
    lmbind(LIGHT0, 1);
    lmbind(LMODEL, 1);
    lmbind(MATERIAL,1);
    
    setnurbsproperty( N_ERRORCHECKING, 1.0 );
    setnurbsproperty( N_PIXEL_TOLERANCE, 100.0 );

    init_trims();
}


/* Draw the nurb, possibly with trimming */
void draw_nurb(Boolean trimming) {

    static double angle = 0.0;
    int i,j;


	/* wave the flag by rotating Z coords though a sine wave */
    for (i=1; i<4; i++)
        for (j=0; j<4; j++)
            ctlpoints[i][j][2] = sin((double)i+angle);

    angle += 0.1;
    
    czclear(0x969696, zfar);

    pushmatrix();

        translate(2.5,-1.0,0.0);
        scale(1.5,1.0,1.0);
        rotate(900,'z');
        rotate(getvaluator(MOUSEY),'x');
        rotate(getvaluator(MOUSEX),'y');
        
        bgnsurface();
            if (trimming) {
                dotrim(whole);
                dotrim(path[0]);
                dotrim(path[1]);
                dotrim(path[2]);
            }
            
            nurbssurface( S_NUMKNOTS, sknots, T_NUMKNOTS, tknots,
                          sizeof(Point) * T_NUMPOINTS, sizeof(Point),
                          &ctlpoints[0][0][0], T_ORDER, S_ORDER, N_V3D);
        endsurface();
        
        draw_hull(ctlpoints);
    
    popmatrix();
    
    swapbuffers();
}

/* Draw the convex hull of the control points */
void draw_hull(Point cpoints[S_NUMPOINTS][T_NUMPOINTS]) {

    int s,t;
    
    cpack(0xFF00);

    for (s=0; s<S_NUMPOINTS; s++)
        for (t=0; t<T_NUMPOINTS-1; t++) {
            bgnline();
                v3d(cpoints[s][t]);
                v3d(cpoints[s][t+1]);
            endline();
        }
        
    for (t=0; t<T_NUMPOINTS; t++)
        for (s=0; s<S_NUMPOINTS-1; s++) {
            bgnline();
                v3d(cpoints[s][t]);
                v3d(cpoints[s+1][t]);
            endline();
        }
}


/* Execute a bgn/endtrim pair for the given trim curve structure. */
void dotrim(TrimCurve *trim_curve) {

    int i;

    bgntrim();
        for (i=0; i<trim_curve->pieces; i++) {

            if (trim_curve->trim[i].type == PWL) {
                    pwlcurve( trim_curve->trim[i].points,
                              &trim_curve->trim[i].point[0][0],
                              sizeof(TrimPoint),
                              N_P2D);
            } else {
                nurbscurve( ELEMENTS(trimknots),trimknots,
                            sizeof(TrimPoint),&trim_curve->trim[i].point[0][0],
                            trim_curve->trim[i].points, N_P2D);
            }
        }
    endtrim();
}

