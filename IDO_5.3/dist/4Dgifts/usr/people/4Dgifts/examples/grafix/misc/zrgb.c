/*
 *   zrgb.c:
 *
 *    This program demostrates zbuffering 3 intersecting RGB polygons while
 *  in doublebuffer mode where, movement of the mouse with the LEFTMOUSE 
 *  button depressed will, rotate the 3 polygons. This is done via compound
 *  rotations allowing continuous screen-oriented rotations. (See orient(),
 *  and draw_scene() below).  Notice the effective way there is no wasted 
 *  CPU usage when the user moves the mouse out of the window without holding
 *  down LEFTMOUSE--there is no qtest being performed and so the program
 *  simply blocks on the qread statement.
 *    Press the "Esc"[ape] key to exit.  
 *     Please note that this program will not work on any 8-bit IRIS
 *  machine.
 *                                          ratman - 1989
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

Matrix objmat = {
    {1.0, 0.0, 0.0, 0.0},
    {0.0, 1.0, 0.0, 0.0},
    {0.0, 0.0, 1.0, 0.0},
    {0.0, 0.0, 0.0, 1.0},
};

Matrix idmat = {
    {1.0, 0.0, 0.0, 0.0},
    {0.0, 1.0, 0.0, 0.0},
    {0.0, 0.0, 1.0, 0.0},
    {0.0, 0.0, 0.0, 1.0},
};


/* Modes the program can be in */
#define NOTHING 0
#define ORIENT 1

int mode = 0;
int omx, mx, omy, my;                             /* old and new mouse position */
float scrnaspect;                            /* aspect ratio value         */
long zfar;               /* holds specific machine's maximum Z depth value */

main(argc, argv)
int argc;
char *argv[];
{
    long dev;
    short val;
    int redrawneeded=TRUE;       /* Is true when the scene needs redrawing */


    initialize(argv[0]);

    while (TRUE) {

        if (redrawneeded) {
            draw_scene();
            redrawneeded=FALSE;
        }

        while (qtest() || (!redrawneeded)) {

            switch(dev=qread(&val)) {

            case ESCKEY:        /* exit when key is going up, not down      */
                if (val)        /* this avoids the scenario where a window  */
                    break;      /* underneath this program's window--say    */
                exit(0);        /* another copy of this program--getting the
                                 * ESC UP event and exiting also.
                                 */
            case REDRAW:
                reshapeviewport();
                redrawneeded=TRUE;
                break;

            case LEFTMOUSE:
                if (val) {
                    mode = ORIENT;
                    omx = mx;
                    omy = my;
                } else 
                    mode = NOTHING;
                break;

            case MOUSEX:
                omx = mx; 
                mx = val;
                if (mode == ORIENT) {
                    update_scene();
                    redrawneeded=TRUE;
                }
                break;

            case MOUSEY:
                omy = my;
                my = val;
                if (mode == ORIENT) {
                    update_scene();
                    redrawneeded=TRUE;
                }
                break;
            }
        }
    }
}


initialize(progname)
char *progname;
{

    long xscrnsize;              /* size of screen in x used to set globals  */
    long testifZinst;


    /*
     * This program requires the following to run:
     *  -- z buffer
     *  -- ability to do double-buffered RGB mode
     */
    /* Test for Z buffer */
    testifZinst = getgdesc(GD_BITS_NORM_ZBUFFER);
    if (testifZinst == FALSE) {
         fprintf(stderr,"BUMmer!--%s won't work on ", progname);
         fprintf(stderr,"this machine--zbuffer option not installed.\n");
         exit(0);
    }
    /* Test for double-buffered RGB */
    if (getgdesc(GD_BITS_NORM_DBL_RED) == 0) {
         fprintf(stderr,"BUMmer!--%s won't work on ", progname);
         fprintf(stderr,"this machine--not enough bitplanes.\n");
         exit(0);
        
    }

    /* Code to keep same aspec ratio as the screen */
    keepaspect(getgdesc(GD_XMMAX), getgdesc(GD_YMMAX));
    scrnaspect = (float)getgdesc(GD_XMMAX)/(float)getgdesc(GD_YMMAX);

    winopen(progname);
    wintitle("Zbuffered RGB #1");

    doublebuffer();
    RGBmode();
    gconfig();
    zbuffer(TRUE);
    glcompat(GLC_ZRANGEMAP, 0);
    zfar = getgdesc(GD_ZMAX);

    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
    qdevice(MOUSEX);
    qdevice(MOUSEY);
}


update_scene() {

    switch (mode) {

        case ORIENT:
            orient();
            break;
    }
}


orient () {

    pushmatrix();

    loadmatrix(idmat);

    rotate(mx-omx, 'y');
    rotate(omy-my, 'x');

    multmatrix(objmat);
    getmatrix(objmat);

    popmatrix();
}


draw_scene() {

    czclear(0x00C86428, zfar);

    perspective(400, scrnaspect, 30.0, 60.0);
    translate(0.0, 0.0, -40.0);
    multmatrix(objmat);
    rotate(-580, 'y');    /* skews original view to show all polygons */
    draw_polys();

    swapbuffers();
}


float polygon1[3][3] = { {-10.0, -10.0,   0.0,},
                         { 10.0, -10.0,   0.0,},
                         {-10.0,  10.0,   0.0,} };

float polygon2[3][3] = { {  0.0, -10.0, -10.0,},
                         {  0.0, -10.0,  10.0,},
                         {  0.0,   5.0, -10.0,} };

float polygon3[4][3] = { {-10.0,   6.0,   4.0,},
                         {-10.0,   3.0,   4.0,},
                         {  4.0,  -9.0, -10.0,},
                         {  4.0,  -6.0, -10.0,} };

draw_polys() {

    bgnpolygon();
    cpack(0x00000000);
    v3f(&polygon1[0][0]);
    cpack(0x007F7F7F);
    v3f(&polygon1[1][0]);
    cpack(0x00FFFFFF);
    v3f(&polygon1[2][0]);
    endpolygon();

    bgnpolygon();
    cpack(0x0000FFFF);
    v3f(&polygon2[0][0]);
    cpack(0x007FFF00);
    v3f(&polygon2[1][0]);
    cpack(0x00FF0000);
    v3f(&polygon2[2][0]);
    endpolygon();

    bgnpolygon();
    cpack(0x0000FFFF);
    v3f(&polygon3[0][0]);
    cpack(0x00FF00FF);
    v3f(&polygon3[1][0]);
    cpack(0x00FF0000);
    v3f(&polygon3[2][0]);
    cpack(0x00FF00FF);
    v3f(&polygon3[3][0]);
    endpolygon();
}
