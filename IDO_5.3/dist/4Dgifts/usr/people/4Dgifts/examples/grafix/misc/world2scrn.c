/*        
 *    world2srcn.c:
 *
 *    This program takes coordinates in 3D and translates them into screen 
 *  (mouse) coordinates for you.  The sample point is one corner of a cube.
 *
 *               G. "Murdock" Helms, SGI Product Support       3/15/91 
 */


#include <gl/gl.h>                 /* standard GL includes */
#include <gl/device.h>

long xorg, yorg, hgt, wid;         /* dimensions of window */
Matrix currmtrx;                   /* needed for matrix manipulations */

main()
{
    short value;                   /* value of device triggered */
    float box[8][3];               /* vertices of box */
    float width;                   /* width of one side of the box */
 
    initialize(box, &width);       /* set up the initial values */

    drawscene(box, &width);        /* draw the box */

    while (TRUE) {                 /* loop until program is killed */

        switch (qread(&value)) {   /* get value on queue */

            case ESCKEY:           /* if escape key, clear and exit */
		if (value) {
                    system("/usr/sbin/gclear");
		    gexit();
                    exit(0); 
                }

            case REDRAW:           /* if a redraw, reshape window and draw */
		if (value) {
                    reshapeviewport();
                    getorigin(&xorg, &yorg);        /* and where it is */
                    drawscene(box, &width);
		}
                break;

        }
    }                /* end while */
}                                          /* end main */


drawscene(box, width)
float box[8][3];
float width;
{
    static int bluecol[3] = {0, 0, 255}; /* i like blue */
    static int blackcol[3] = { 0, 0, 0}; /* black outline */


    c3i(bluecol);                /* clear the whole window to blue */
    clear();
 
    c3i(blackcol);               /* now change to black */
    drawbeam(box);               /* and draw the outline */

    wtoscrn(1.0, 1.0, -1.0);     /* these are world coords, change to screen */
}


/* 
 * The following section is brazenly stolen from some
 * of SGI's Intro to Graphics class material, namely,
 * the old library that had definitions of boxes and torii.
 * For our example, we will be working with point 2, which
 * translates in worldspace into 1,1,-1 if initialized with
 * the value 2,2,2.  I did this on purpose for simplicity's
 * sake.
 */

/*        initbeam (x, y, z, point)
 *        x is the length of the base.  y is the height.  z is the depth.
 *        point is an array of 8 (x, y, z) vertices.
 *        This subroutine calculates and returns the coordinates for the eight
 *        vertices of a beam which is centered at (x/2.0, 0.0, 0.0).
 */
initbeam (x, y, z, point)
register float x, y, z, point[][3];
{
    point[0][0] = point[3][0] = point[4][0] = point[5][0] = -x/2.0;
    point[1][0] = point[2][0] = point[6][0] = point[7][0] =  x/2.0;
    point[4][1] = point[5][1] = point[6][1] = point[7][1] = -y/2.0;
    point[0][1] = point[1][1] = point[2][1] = point[3][1] =  y/2.0;
    point[2][2] = point[3][2] = point[5][2] = point[7][2] = -z/2.0;
    point[0][2] = point[1][2] = point[4][2] = point[6][2] =  z/2.0;

}



/*        drawbeam (point)
 *        point is the array of the coordinates of 8 vertices.
 *        This subroutine draws a wire frame beam.
 */
drawbeam (point)
register float point[][3];
{

        /*  draw bottom facing polygon      */
    bgnclosedline();
    v3f( point[4] );
    v3f( point[6] );
    v3f( point[7] );
    v3f( point[5] );
    endclosedline();

        /*  draw left facing polygon        */
    bgnclosedline();
    v3f( point[3] );
    v3f( point[0] );
    v3f( point[4] );
    v3f( point[5] );
    endclosedline();

        /*  draw right facing polygon       */
    bgnclosedline();
    v3f( point[2] );
    v3f( point[1] );
    v3f( point[6] );
    v3f( point[7] );
    endclosedline();

        /*  draw top facing polygon         */
    bgnclosedline();
    v3f( point[0] );
    v3f( point[1] );
    v3f( point[2] );
    v3f( point[3] );
    endclosedline();

        /*  draw front facing polygon       */   

    bgnclosedline();
    v3f( point[3] );
    v3f( point[2] );
    v3f( point[7] );
    v3f( point[5] );
    endclosedline();

        /*  draw back facing polygon        */
    bgnclosedline();
    v3f( point[0] );
    v3f( point[1] );
    v3f( point[6] );
    v3f( point[4] );
    endclosedline();
}


initialize(box, width)
register float box[8][3];           /*  The eight vertices of the beam  */
register float *width;
{
    float aspect;               
    int gid;                        
    register float height, length;  /* needed for square initialization */


    prefsize(getgdesc(GD_XPMAX)/3, getgdesc(GD_YPMAX)/3);
    winopen ("World to Screen");    
    RGBmode();                      /* I like c3i calls, no map dependencies */
    gconfig();                      /* configure the hardware */

    qdevice (ESCKEY);               /* queue the "EXIT" key */


    aspect = ((float) getgdesc(GD_XPMAX) / (float) getgdesc(GD_YPMAX));
    perspective (450, aspect, 0.1, 100.0);
    polarview (5.0, 0, 0, 0);       
    
    getmatrix(currmtrx);            /* we need a copy to work on */
    getsize(&wid, &hgt);            /* and we need to know window size */
    getorigin(&xorg, &yorg);        /* and where it is */

    *width = 2.0; height = 2.0; length = 2.0;  /* static info for size of sqr */
    initbeam (*width, height, length, box);    /* init square */
}


wtoscrn(x, y, z)                    /* This is the core of the program */
float  x, y, z;                     /* these are your world coords */
{

/* The theory behind this, as explained by thant, is that you 
 * need to perform any perspective, lookat, translate calls you 
 * want to do before you draw the figure.  Then, right before
 * you draw, call getmatrix to return your current matrix.  Also
 * call getsize and getorigin to get window values.
 * 
 * Then you take your xyz world coordinates and feed them through
 * the matrix that getmatrix gave you (the one with perspective, lookat,
 * et al in it).  The section below performs that for you.
 * 
 * Once you have the new values (newx,newy and newz), turn them 
 * to screen coords by using the following algorithm:
 *
 *          (xnew+1)/2 * size + origin = screen x
 */ 

    Matrix projmtrx; 
    float xnew, ynew, znew, wnew;
    long savid, savmode;
    short left, right, bottom, top;
    int dolight = 0;

    printf("\n (x,y,z) World coords are: (%.2f,%.2f,%.2f)\n", x, y, z);
    xnew = x*currmtrx[0][0] + y*currmtrx[1][0] +
           z*currmtrx[2][0] + currmtrx[3][0];

    ynew = x*currmtrx[0][1] + y*currmtrx[1][1] +
           z*currmtrx[2][1] + currmtrx[3][1];

    znew = x*currmtrx[0][2] + y*currmtrx[1][2] +
           z*currmtrx[2][2] + currmtrx[3][2];

    wnew = x*currmtrx[0][3] + y*currmtrx[1][3] +
           z*currmtrx[2][3] + currmtrx[3][3];

    xnew = xnew/wnew;        /* wnew is an identity that you need */
    ynew = ynew/wnew;

    x = (int) ((xnew+1)/2 * wid + xorg);
    y = (int) ((ynew+1)/2 * hgt + yorg);

    printf("       and now, <-*Presto!*->:\n");
    printf(" (x,y) transformed Screen coords of the\n upper right ");
    printf("\"inside\" corner are: (%d,%d)\n\n",(int) x,(int)y);
}
