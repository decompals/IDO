/*
 *   zrgbmenu.c:
 *
 *    This program implements a combination of various GL features:
 *
 *       doublebuffered-RGB mode zbuffering, with a pop-up menu.
 *
 *  It further implements--either manually via LEFTMOUSE, or in an
 *  "automatic" (i.e. animation) mode format--movement of the polygons 
 *  via compound rotations to allow for continuous screen-oriented 
 *  rotations (see orient(), spin(), and draw_scene() below).  Horizontal
 *  mouse movement rotates the polygons about the y-axis where right is 
 *  positive and left is negative.  Vertical mouse movement rotates the
 *  polygons about the x-axis where down is positive and up is negative.
 *
 *                                            ratman - 1989
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
#define ORIENT  1
#define SPIN    2

/* Values returned from popup menu */
#define MANUAL    1
#define AUTOMATIC 2

int mode = 0;
int menu;
int omx, mx, omy, my;                         /* old and new mouse position */
float scrnaspect;                             /* aspect ratio value         */
long zfar;                /* holds specific machine's maximum Z depth value */

main(argc, argv)
int argc;
char *argv[];
{
    long dev;
    short val;
    long menuval;
    int redrawneeded=TRUE;        /* Is true when the scene needs redrawing */


    initialize(argv[0]);

    while (TRUE) {

        if (redrawneeded) {
            if (mode == SPIN) {
                update_scene();
                draw_scene();
                /*
                 * Note: Don't reset redrawneeded, so we keep on
                 * spinning.
                 */
            } else {
                draw_scene();
                redrawneeded=FALSE;
            }
        }

        while (qtest() || (!redrawneeded)) {

            switch(dev=qread(&val)) {

                case ESCKEY:     /* Exit when key is going up, not down.    */
                    if (val)     /* This avoids the scenario where a window */ 
                        break;   /* underneath this program's window--say a */
                    exit(0);     /* demo--would otherwise get the up-event  */
                                 /* of the Esc key and exit. */

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

                case RIGHTMOUSE:
                    if (val) {
                        menuval = dopup(menu);
                        switch(menuval) {
                            case MANUAL:
                                mode = NOTHING;
                                break;
                            case AUTOMATIC:
                                mode = SPIN;
                                redrawneeded=TRUE;
                                break;
                        }
                    }
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
    wintitle("Zbuffered RGB #2");

    doublebuffer();
    RGBmode();
    gconfig();
    zbuffer(TRUE);
    glcompat(GLC_ZRANGEMAP, 0);
    zfar = getgdesc(GD_ZMAX);

    qdevice(ESCKEY);
    qdevice(RIGHTMOUSE);
    qdevice(LEFTMOUSE);
    qdevice(MOUSEX);
    qdevice(MOUSEY);

    menu = defpup("Zbuffered-RGB #2 %t|Manual Mode|Automatic Mode");

}


update_scene() {

    switch (mode) {

        case ORIENT:
            orient();
            break;
        case SPIN:
            spin();
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


spin () {

    pushmatrix();

    loadmatrix(idmat);

    rotate(5, 'x');            
    rotate(6, 'y');           
    rotate(4, 'z');          

    multmatrix(objmat);
    getmatrix(objmat);

    popmatrix();

}


draw_scene() {

    czclear(0x0C86428, zfar);

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
