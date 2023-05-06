/*
 * depthcue.c:
 *
 *   draws a depthcue'd 3-d wireframe cube with lots 'a little points inside.
 *   moving the mouse, rotates the cube.  NEAR and FAR (Z) clip-planes are
 *   currently set to 350.0 and 1000.0, respectively.  Give the command line
 *   2 extra arguements:  floating point near and far values to change where
 *   Z gets clipped, which will also alter the distribution of brightness to
 *   darkness of the visual depth cues.
 */

#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>

float hrand();

main (argc,argv)
int argc;
char **argv;
{
    int i;
    long znear, zfar;
    long dev;
    short val;
    float near, far;

    foreground();
    winopen("test");
    doublebuffer();
    gconfig();
    if (argc == 3)  {
        near = atof(argv[1]);
        far = atof(argv[2]);
    }
    else {
        near = 350.0;
        far = 1000.0;
    }
    reshapeviewport();
    perspective(600, 1.0, near, far);
    lookat(0.0, 0.0, 700.0, 0.0, 0.0, 0.0, 0);
    qdevice(ESCKEY);

    makeobj(1);

               /* generate a bunch of random points */
        for (i = 0; i < 100; i++)
            pnt(hrand(-200.0,200.0), hrand(-200.0,200.0), hrand(-200.0,200.0));

               /* and a cube */
        movei(-200, -200, -200);
        drawi( 200, -200, -200);
        drawi( 200,  200, -200);
        drawi(-200,  200, -200);
        drawi(-200, -200, -200);
        drawi(-200, -200,  200);
        drawi(-200,  200,  200);
        drawi(-200,  200, -200);
        movei(-200,  200,  200);
        drawi( 200,  200,  200);
        drawi( 200, -200,  200);
        drawi(-200, -200,  200);
        movei( 200,  200,  200);
        drawi( 200,  200, -200);
        movei( 200, -200, -200);
        drawi( 200, -200,  200);
    closeobj();

/* load the color map with a cyan ramp */
    for (i = 0; i <= 127; i++)
        mapcolor(128+i, 0, 2*i, 2*i);

/* set up the mapping of z values to color map indices:
   znear is mapped to index 128 and zfar is mapped to index 255 */
    glcompat(GLC_ZRANGEMAP, 0);
    znear = getgdesc(GD_ZMIN);
    zfar  = getgdesc(GD_ZMAX);
    lshaderange(128, 255, znear, zfar);

/* turn on depthcue mode:  the color index of each pixel in points
   and lines is determined from the z value of the pixel */
    depthcue(1);

/* until a key is pressed, rotate cube according to movement of the mouse */
    while (TRUE) {
        if (qtest()) {
            dev = qread(&val);
            switch(dev) {
                case ESCKEY:
                    gexit();
                    exit(0);
                    break;
                case REDRAW:
                    reshapeviewport();
                    perspective(600, 1.0, near, far);
                    lookat(0.0, 0.0, 700.0, 0.0, 0.0, 0.0, 0);
                    break;
	    }
        }
        pushmatrix();
        rotate(3*getvaluator(MOUSEY), 'x');
        rotate(3*getvaluator(MOUSEX), 'y');
        color(BLACK);
        clear();
        callobj(1);
        popmatrix();
        swapbuffers();
    }
}

/* this routine returns random numbers in the specified range */
float hrand(low,high)
float low,high;
{
    float val;

    val = ((float)( (short)rand() & 0xffff)) / ((float)0xffff);
    return( (2.0 * val * (high-low)) + low);
}
