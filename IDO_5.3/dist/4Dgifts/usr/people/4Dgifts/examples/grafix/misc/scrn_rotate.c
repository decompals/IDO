/*
 *                               scrn_rotate.c
 *
 *     This program illustrates a technique for rotating an object about a 
 *  fixed set of axes (screen axes x, y, and z).    It also demonstrates a 
 *  technique for doing backface elimination depending upon the visual re-
 *  lationship between the eyepoint and a six-sided cube. 
 *
 *  NOTE: if compiled with the "debug" flag "-DBACKFACE" the graphics lib-
 *        brary function backface will replace the chunk of code ensuing 
 *        from the function norm_dot and beyond.
 *
 *                          Paul Mlyniec and David Ratcliffe - 1986
 */

#include <stdio.h>
#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>

Coord ident [4][4] =     { 1.0, 0.0, 0.0, 0.0,    /* identity matrix */
                           0.0, 1.0, 0.0, 0.0,
                           0.0, 0.0, 1.0, 0.0,
                           0.0, 0.0, 0.0, 1.0};

static Coord cm [4][4] = { 1.0, 0.0, 0.0, 0.0,    /* cumulative matrix */
                           0.0, 1.0, 0.0, 0.0,
                           0.0, 0.0, 1.0, 0.0,
                           0.0, 0.0, 0.0, 1.0};

/*  Define the sides of the cube in world coordinates. */

static Coord pfrnt[4][3] = {{    0.0,    0.0,    0.0},
                            {  100.0,    0.0,    0.0},
                            {  100.0,  100.0,    0.0},
                            {    0.0,  100.0,    0.0}};
    
static Coord pback[4][3] = {{    0.0,    0.0, -100.0},
                            {    0.0,  100.0, -100.0},
                            {  100.0,  100.0, -100.0},
                            {  100.0,    0.0, -100.0}};
    
static Coord ptop[4][3] =  {{    0.0,  100.0,    0.0},
                            {  100.0,  100.0,    0.0},
                            {  100.0,  100.0, -100.0},
                            {    0.0,  100.0, -100.0}};
    
static Coord pbot[4][3] =  {{    0.0,    0.0,    0.0},
                            {    0.0,    0.0, -100.0},
                            {  100.0,    0.0, -100.0},
                            {  100.0,    0.0,    0.0}};
    
static Coord prsid[4][3] = {{  100.0,    0.0,    0.0},
                            {  100.0,    0.0, -100.0},
                            {  100.0,  100.0, -100.0},
                            {  100.0,  100.0,    0.0}};
    
static Coord plsid[4][3] = {{    0.0,    0.0,    0.0},
                            {    0.0,  100.0,    0.0},
                            {    0.0,  100.0, -100.0},
                            {    0.0,    0.0, -100.0}};

Coord x, y, z;
Angle rx, ry, rz;
float norm_dot();
long xmaxscrn, ymaxscrn;

main() {

    int i, j;
    long dev;
    short data;
    float scrnaspect;            /* aspect ratio value */


/*  initialize and set the display to double buffer mode */

    xmaxscrn = getgdesc(GD_XPMAX)-1;
    ymaxscrn = getgdesc(GD_YPMAX)-1;
    keepaspect(xmaxscrn, ymaxscrn);
    scrnaspect = ((float)xmaxscrn)/ymaxscrn;
    winopen("screeen rotation prog.");
    doublebuffer();
    gconfig();
    writemask((1<<getplanes())-1);

    qdevice(PAD1);    /* translate (in Z) toward the eyepoint */
    qdevice(PAD2);    /* rotate about the X axis in a negative direction */
    qdevice(PAD3);    /* translate (in Z) away from the eyepoint */
    qdevice(PAD4);    /* rotate about the Y axis in a positive direction */
    qdevice(PAD5);    /* reset rotations and translations to default */
    qdevice(PAD6);    /* rotate about the Y axis in a negative direction */
    qdevice(PAD7);    /* rotate about the Z axis in a positive direction */
    qdevice(PAD8);    /* rotate about the X axis in a positive direction */
    qdevice(PAD9);    /* rotate about the Z axis in a negative direction */
    qdevice(FKEY);    /* translate (in Z) toward the eyepoint */
    qdevice(BKEY);    /* translate (in Z) away from the eyepoint */
    qdevice(ESCKEY);  /* exit program */

#ifdef BACKFACE    /* compile with "-DBACKFACE" if you desire to use GL's */
    backface(TRUE);
#endif

    perspective(470,scrnaspect,1.0,10000.0); 

/*  initialize the modeling transformation values          */
    rx =     0;  ry =     0;  rz =      0;
     x = -50.0;   y = -50.0;   z = -400.0;


/*  set up the loop for reading input and drawing the cube    */

    while(1) {
        color(BLACK);
        clear();
        gflush();                                    /*  get clear going */
        viewcube();
    
        /*  read the input for moving the box around the eye point */
        while (qtest()) {
            dev = qread(&data);
            switch (dev) {
                case REDRAW:                             /* redraw event */
                    reshapeviewport();
                    viewcube('t');
                    break;

                case(ESCKEY):                            /* exit program */
                    gexit();
                    exit(0);
                    break;

                case(FKEY):             /* translate toward the eyepoint */
                case(PAD1): 
                    while(getbutton(FKEY) || getbutton(PAD1)) {
                        z = z + 20.0;
                        viewcube('t');
                    }
                    break;

                case(BKEY):           /* translate away from the eyepoint */
                case(PAD3): 
                    while(getbutton(BKEY) || getbutton(PAD3)) {
                        z = z - 20.0;
                        viewcube('t');
                    }
                    break;
    
                case(PAD2): 
                    while(getbutton(PAD2)) {   /* rotate about the X axis */
                        rx = rx - 100;         
                        viewcube('x');
                    }
                    updatemat('x');     /* incorporate this rotation into */
                    rx = 0;                 /* cumulative rotation matrix */
                    break;      
    
                case(PAD4): 
                    while(getbutton(PAD4)) {   /* rotate about the Y axis */
                          ry = ry + 100;         
                          viewcube('y');
                    }
                    updatemat('y');     /* incorporate this rotation into */
                    ry = 0;                 /* cumulative rotation matrix */
                    break;      
    
                case(PAD6): 
                    while(getbutton(PAD6)) {   /* rotate about the Y axis */
                        ry = ry - 100;          
                        viewcube('y');
                    }
                    updatemat('y');     /* incorporate this rotation into */
                    ry = 0;                 /* cumulative rotation matrix */
                    break;     
    
                case(PAD7): 
                    while(getbutton(PAD7)) {   /* rotate about the Z axis */
                        rz = rz + 100;         
                        viewcube('z');
                    }
                    updatemat('z');     /* incorporate this rotation into */
                    rz = 0;                 /* cumulative rotation matrix */
                    break;     

                case(PAD8): 
                    while(getbutton(PAD8)) {   /* rotate about the X axis */
                        rx = rx + 100;         
                        viewcube('x');
                    }
                updatemat('x');         /* incorporate this rotation into */
                rx = 0;                     /* cumulative rotation matrix */
                break;      

                case(PAD9): 
                    while(getbutton(PAD9)) {   /* rotate about the Z axis */
                        rz = rz - 100;         
                        viewcube('z');
                    }
                    updatemat('z');     /* incorporate this rotation into */
                    rz = 0;                 /* cumulative rotation matrix */
                    break;     
    
                case(PAD5):           /* reset rotations & translations   */
                    x  =  -50.0;           
                    y  =  -50.0;                 
                    z  = -400.0;            
                    rx = 0;                      
                    ry = 0; 
                    rz = 0;

                    for(i=0;i<4;i++) {
                        for(j=0;j<4;j++)
                            cm[i][j] = ident[i][j];
                    }

                    viewcube('t');
                    break;

            }   /* end switch */

            qreset();

        }                  /* end while(qtest()) */

    }                                           /* end while(1) */

}                                                            /* end main */

viewcube(axis) 
char axis;
{

/*  transform the cube in world space and (if BACKFACE not defined,
    in software, ) check each face for back face elimination
*/

    color(BLACK);
    clear();
    gflush();

    pushmatrix();
    translate(x,y,z);
    pushmatrix();
    translate(50.0,50.0,-50.0);

/* apply rotation about a single axis */
    switch(axis) {
        case('x'): 
            rotate(rx,'x');
            break;
        case('y'): 
            rotate(ry,'y');
            break;
        case('z'): 
            rotate(rz,'z');
            break;
        case('t'): 
            break;
    }

/* apply all prior rotations */
    multmatrix(cm);
    translate(-50.0,-50.0,50.0);

#ifdef BACKFACE    /* compile with "-DBACKFACE" if you desire to use GL's */
    color(1);
    polf(4,pfrnt);
    color(2);
    polf(4,pback);
    color(3);
    polf(4,ptop);
    color(4);
    polf(4,pbot);
    color(5);
    polf(4,prsid);
    color(6);
    polf(4,plsid);
#else
    color(1);
    if(norm_dot(pfrnt) >= 0.0) polf(4,pfrnt);
    color(2);
    if(norm_dot(pback) >= 0.0) polf(4,pback);
    color(3);
    if(norm_dot(ptop) >= 0.0) polf(4,ptop);
    color(4);
    if(norm_dot(pbot) >= 0.0) polf(4,pbot);
    color(5);
    if(norm_dot(prsid) >= 0.0) polf(4,prsid);
    color(6);
    if(norm_dot(plsid) >= 0.0) polf(4,plsid);
#endif

    popmatrix(); 
    popmatrix();
    swapbuffers();
}

/*
 *  Function to postmultiply cumulative rotations 
 *     by rotation about a single axis 
 */

updatemat(axis) 
char axis;
{

    pushmatrix();
    loadmatrix(ident);

    switch (axis) {
        case ('x'): 
            rotate(rx,'x');
            break;
        case ('y'): 
            rotate(ry,'y');
            break;
        case ('z'): 
            rotate(rz,'z');
            break;
    }
    
    multmatrix(cm);
    getmatrix(cm);
    popmatrix();

}

/*  This function takes as input an array of points in homogeneous coord.
    which make up a surface or plane.  The unit normal of the surface and
    the eyepoint to surface unit vector are computed and the dot product
    is calculated. norm_dot returns the dot product floating point value
    and the transformed points for the surface.
*/

float norm_dot(passpoly)
Coord passpoly[][3];
{
    int i;
    float a[3],b[3],c[3],d,abs;
#ifdef 3000
    float fabs();
#endif
    Coord postrans [4][3];


/*  apply the current transformation to the surface points    */
    transform(4,passpoly,postrans);

/*  determine two vectors which lie in the specified plane.  The first three
    points are taken from the surface array.  p1, p2, p3 are ordered by the 
    right-hand rule in the right-hand coordinate system:  i.e. points ordered 
    counter-clockwise when on the positive side of the plane or surface are 
    visible--not backfacing--surfaces.
    a[] gets the xyz coords of row 2
    b[] gets the xyz coords of row 0.
*/
/* 1]  determine two vectors.  note this routine assumes they are not in-line
*/
    for(i = 0; i < 3; i++)
        a[i] = postrans[2][i] - postrans[1][i];

    for(i = 0; i < 3; i++)
        b[i] = postrans[0][i] - postrans[1][i];

/* 2]  find the cross product of the two vectors */

    c[0] = a[1] * b[2] - a[2] * b[1];
    c[1] = a[2] * b[0] - a[0] * b[2];
    c[2] = a[0] * b[1] - a[1] * b[0];


/* 3]  calculate the unit normal vector for the plane or poly using the square
    root of the sum of the squares of x, y, and z to determine length of vec-
    tor, then dividing each axis by that length (x/l, y/l, z/l).
*/
    abs = 0.0;
    
    for (i = 0; i < 3; i++) 
        abs += (c[i]*c[i]);
    d = sqrt(abs);

    if (fabs(d) > 0.000001)    {
        for (i = 0; i < 3; i++)
            a[i] = c[i]/d;

/* 4] calculate the unit vector pointing from the eyepoint to the normal of 
    the plane or poly
*/
        abs = 0.0;

        for (i = 0; i < 3; i++) 
            c[i] = postrans[1][i];
        for (i = 0; i < 3; i++) 
            abs = abs + (c[i]*c[i]);
        d = sqrt(abs); 
    
        if (fabs(d) > 0.000001)    {
            for (i = 0; i < 3; i++)
                b[i] = c[i]/d;

/* 5] return the dot product between the eye vector and the plane normal */
            for (i = 0, d=0.0; i < 3; i++)
                d = d + a[i]*b[i];
        }
        else 
             printf("\n\n Magnitude of surface vector is zero!");
    }
    else 
        printf("\n\n Magnitude of eye vector is zero!");
    return(d);
}


/* the transform function.  this demonstrates what would be the equivalent of
   using feedback with xfpt() on the 2000/3000 series IRIS machines: 
   i.e.  simply multiply each vertex point with the current transformtion
   matrix without any clipping, scaling, etc. to derive transformed world
   coordinate values.
*/

transform(n,passpoly,postrans)
long n;
Coord passpoly[][3], postrans[][3];
{
    Matrix ctm;

    pushmatrix();
    getmatrix(ctm);

    postrans[0][0] = passpoly[0][0]*ctm[0][0] + 
                     passpoly[0][1]*ctm[1][0] +
                     passpoly[0][2]*ctm[2][0] + ctm[3][0];
    postrans[0][1] = passpoly[0][0]*ctm[0][1] + 
                     passpoly[0][1]*ctm[1][1] +
                     passpoly[0][2]*ctm[2][1] + ctm[3][1];
    postrans[0][2] = passpoly[0][0]*ctm[0][2] +
                     passpoly[0][1]*ctm[1][2] +
                     passpoly[0][2]*ctm[2][2] + ctm[3][2];
          
    postrans[1][0] = passpoly[1][0]*ctm[0][0] + 
                     passpoly[1][1]*ctm[1][0] +
                     passpoly[1][2]*ctm[2][0] + ctm[3][0];
    postrans[1][1] = passpoly[1][0]*ctm[0][1] + 
                     passpoly[1][1]*ctm[1][1] +
                     passpoly[1][2]*ctm[2][1] + ctm[3][1];
    postrans[1][2] = passpoly[1][0]*ctm[0][2] +
                     passpoly[1][1]*ctm[1][2] +
                     passpoly[1][2]*ctm[2][2] + ctm[3][2];
                     
    postrans[2][0] = passpoly[2][0]*ctm[0][0] + 
                     passpoly[2][1]*ctm[1][0] +
                     passpoly[2][2]*ctm[2][0] + ctm[3][0];
    postrans[2][1] = passpoly[2][0]*ctm[0][1] + 
                     passpoly[2][1]*ctm[1][1] +
                     passpoly[2][2]*ctm[2][1] + ctm[3][1];
    postrans[2][2] = passpoly[2][0]*ctm[0][2] +
                     passpoly[2][1]*ctm[1][2] +
                     passpoly[2][2]*ctm[2][2] + ctm[3][2];
                     
    postrans[3][0] = passpoly[3][0]*ctm[0][0] + 
                     passpoly[3][1]*ctm[1][0] +
                     passpoly[3][2]*ctm[2][0] + ctm[3][0];
    postrans[3][1] = passpoly[3][0]*ctm[0][1] + 
                     passpoly[3][1]*ctm[1][1] +
                     passpoly[3][2]*ctm[2][1] + ctm[3][1];
    postrans[3][2] = passpoly[3][0]*ctm[0][2] +
                     passpoly[3][1]*ctm[1][2] +
                     passpoly[3][2]*ctm[2][2] + ctm[3][2];
                     
    popmatrix();
}
