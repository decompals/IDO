/*
 *  zbuffer1.c:
 *
 *    simplistic zbuffer demo program that draws two intersecting planes.
 *
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

long zfar;

main(argc,argv)
int argc;
char *argv[];
{
    Device dev;
    short val;


    initialize(argv[0]);

    while (TRUE) {
        if (qtest()) {
            dev = qread(&val);
            switch (dev) {
                case ESCKEY:
                    zbuffer(FALSE);
                    gexit();
                    exit(0);
                    break;
                case REDRAW:
                    reshapeviewport();
                    drawpolys();
                    break;
            }
        }
    }
}

initialize(progname)
char *progname;
{
    int gid;
    long testifZinst;
    long xmaxscrn, ymaxscrn;     /* maximum size of screen in x and y        */
    float scrnaspect;            /* aspect ratio value                       */


    testifZinst = getgdesc(GD_BITS_NORM_ZBUFFER);
    if (testifZinst == FALSE) {
         fprintf(stderr,"BUMmer!--%s won't work on ", progname);
         fprintf(stderr,"this machine--zbuffer option not installed.\n");
         exit(0);
    }

    xmaxscrn = getgdesc(GD_XPMAX)-1;
    ymaxscrn = getgdesc(GD_YPMAX)-1;
    scrnaspect = ((float)xmaxscrn)/ymaxscrn;
    prefposition(xmaxscrn/4,xmaxscrn*3/4,ymaxscrn/4,ymaxscrn*3/4);
    gid = winopen("zbuffer1");
    winconstraints();

    perspective(900, scrnaspect, 1.01, 500.0);
    lookat(-150.0, 90.0, 250.0, 50.0, 50.0, 0.0, 0);

    qdevice(ESCKEY);
    qenter(REDRAW,gid);

    zbuffer(TRUE);
    glcompat(GLC_ZRANGEMAP, 0);
    zfar = getgdesc(GD_ZMAX);
}


drawpolys()
{
    czclear(BLACK, zfar);

    color(YELLOW);
    pmv(0.0, 0.0, 100.0);
    pdr(100.0, 0.0, 100.0);
    pdr(100.0, 100.0, 100.0);
    pdr(0.0, 100.0, 100.0);
    pclos();

    color(RED);
    pmv(0.0, 0.0, 50.0);
    pdr(100.0, 0.0, 50.0);
    pdr(100.0, 100.0, 200.0);
    pdr(0.0, 100.0, 200.0);
    pclos();
}
