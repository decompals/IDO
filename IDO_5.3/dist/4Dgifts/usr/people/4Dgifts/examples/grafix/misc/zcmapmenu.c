/*
 *   zcmapmenu.c:
 *
 *     This program implements a combination of various GL features:
 *
 *       doublebuffered color-map mode zbuffering, with a pop-up menu.
 *
 *   as well as movement of the polygons via compound rotations to allow for
 *   continuous screen-oriented rotations (see orient(), spin(), and 
 *   draw_scene() below).  Horizontal/vertical mouse movement rotates the 
 *   polygons about the y/x -axes.
 *
 *   This program also demonstrates a standard approach to writing code
 *   that does not eat up CPU cycles in polling loops if the mouse is 
 *   elsewhere than over this window and/or if the mouse isn't moving.  
 *   (Past examples have included qtest() or getbutton() cycling that can 
 *   cost a lot of CPU time).  See skeleton.c in this same directory for 
 *   the "template" standalone program that was used to implement the
 *   catching and processing of input from the GL event queue.
 *
 *   see the README.zcmapmenu for more implementation details.
 *
 *                                    dave ratcliffe - 1989
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

#define STARTINDEX  640
#define ENDINDEX    735
#define BACKGROUND  736   

/* Modes the program can be in */
#define NOTHING 0
#define ORIENT  1
#define SPIN    2

/* Values returned from popup menu */
#define MANUAL    1
#define AUTOMATIC 2

int mode = 0;
int menu;
short red[97], gre[97], blu[97];   /* stores original c-map range 32 to 127 */
int omx, mx, omy, my;                         /* old and new mouse position */
float scrnaspect;                             /* aspect ratio value         */
long zfar;                /* holds specific machine's maximum Z depth value */

main(argc, argv)
int argc;
char *argv[];
{
    short val;
    long dev;
    long i, j;
    long menuval;
    long lastmode;
    int redrawneeded=TRUE;    /* Is true when the scene needs redrawing */


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

                                /* Exit when key is going up, not down     */
                                /* This avoids the scenario where a window */ 
            case ESCKEY:        /* underneath this program's window--say a */
                if (val)        /* demo--would otherwise get the up-event  */
                    break;      /* of the Esc key and exit. */
            case WINQUIT:       /* Also, regarding the usage of WINQUIT, 
                                 * which will be put into the queue upon 
                                 * "boinking" on the lighting bolt or by
                                 * choosing the "Quit" NeWS menu item:
                                 * notice how it DOES NOT test to see if
                                 * the WINQUIT val is true because val
                                 * contains the graphics ID of the window 
                                 * being "quitted" and thus will always 
                                 * be non-zero.
                                 */
                for (i=STARTINDEX, j=0; i<=BACKGROUND; i++, j++)
                    mapcolor(i, red[j], gre[j], blu[j]);
                gexit();
                exit(0);
                break;

            case REDRAW:
                reshapeviewport();
                redrawneeded=TRUE;
                break;

            case INPUTCHANGE:    /* testing for INPUTCHANGE is explained */
                if (val) {               /* up in the beginning comments */
                    mode = lastmode;
                    buildmap();
                    if (mode == SPIN) 
                        redrawneeded=TRUE;
                } else {
                    lastmode = mode;
                    mode = NOTHING;
                }
                break;

            case LEFTMOUSE:
                if (val) {
                    omx = mx;
                    omy = my;
                    mode = ORIENT;
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

    long testifZinst;
    register int i, j;
    short redval, greval, bluval;
    long xscrnsize;              /* size of screen in x used to set globals  */


    /*
     * This program requires the following to run:
     *  -- z buffer
     *  -- ability to write color indices STARTINDEX - BACKGROUND
     *     in double-buffered colorindex mode
     */
    /* Test for Z buffer */
    testifZinst = getgdesc(GD_BITS_NORM_ZBUFFER);
    if (testifZinst == FALSE) {
         fprintf(stderr,"BUMmer!--%s won't work on ", progname);
         fprintf(stderr,"this machine--zbuffer option not installed.\n");
         exit(0);
    }
    /* Test for enough color bitplanes */
    if ((1 << getgdesc(GD_BITS_NORM_DBL_CMODE)) <= BACKGROUND) {
         fprintf(stderr,"BUMmer!--%s won't work on ", progname);
         fprintf(stderr,"this machine--not enough color bitplanes.\n");
         exit(0);
        
    }

    /* Code to keep same aspec ratio as the screen */
    keepaspect(getgdesc(GD_XMMAX), getgdesc(GD_YMMAX));
    scrnaspect = (float)getgdesc(GD_XMMAX)/(float)getgdesc(GD_YMMAX);

    winopen(progname);
    wintitle("Zbuffered color-map #3");
    doublebuffer();
    gconfig();
    zbuffer(TRUE);
    glcompat(GLC_ZRANGEMAP, 0);
    zfar = getgdesc(GD_ZMAX);

    /*
     * stay from possible z wrap around during iteration
     */
    lsetdepth(100, zfar - 100);

    qdevice(ESCKEY);
    qdevice(RIGHTMOUSE);
    qdevice(LEFTMOUSE);
    qdevice(MOUSEX);
    qdevice(MOUSEY);
    qdevice(WINQUIT);
    qdevice(WINSHUT);

    menu = defpup("Zbuffered Color-Map #3 %t|Manual Mode|Automatic Mode");

  /* first save out the original color map values between 32 and 127 */
    for (i=STARTINDEX, j=0; i<=BACKGROUND; i++, j++) {
        getmcolor(i, &redval, &greval, &bluval);
        red[j] = redval;
        gre[j] = greval;
        blu[j] = bluval;
    }

    buildmap();
}


buildmap() {

    mapcolor (BACKGROUND, 40, 100, 200);
    /* now build our 3 color ramps of 32 shades that make ranges of: 
     *     32-63 :    YELLOW to BLUE
     *     64-95 :    CYAN   to RED
     *     96-127:    GREEN  to MAGENTA
     */
    rampup(STARTINDEX,    STARTINDEX+31, 255,   0, 255,   0,   0, 255); 
    rampup(STARTINDEX+32, STARTINDEX+63,   0, 255,   0, 255,   0, 255);
    rampup(STARTINDEX+64,      ENDINDEX,   0, 255,   0,   0,   0, 255);
}


#define round(n)     ((int) (n + 0.5))
/*
 *  rampup:
 *    make a bi-linear interpolated color ramp taking as input the 
 *    starting and ending look-up table indices, and the minimum 
 *    and maximum values for red, green, and blue
 */
rampup(first_lutv,last_lutv,minR,maxR,minG,maxG,minB,maxB)
unsigned short first_lutv, last_lutv,         /* start & end ramp values */
               minR, maxR, minG, maxG, minB, maxB;     /* lo/hi rgb vals */
{
    unsigned short len_red, len_green, len_blue, /* length of each color */
                   i;                     /* counter for number of steps */

    short red, gre, blu;                                   /* lut values */
    float rdx, gdx, bdx,                      /* sizes of rgb increments */
          r, g, b,                             /* a position on the ramp */
          steps;                    /* # of steps along the ramp @ which */
                                   /* intensity assignments will be made */

    steps = (float) (last_lutv-first_lutv + 1);/*determine length of ramp*/

    len_red   = (maxR - minR) + 1;            /* determine length of red */
    len_green = (maxG - minG) + 1;          /* determine length of green */
    len_blue  = (maxB - minB) + 1;           /* determine length of blue */

    rdx = (float) len_red   / steps;                     /* compuke step */
    gdx = (float) len_green / steps;                     /* sizes of r g */
    bdx = (float) len_blue  / steps;                     /* and b values */

    r = minR;                                         /* assign starting */
    g = minG;                                        /* indices for each */
    b = minB;                                        /* color value      */

    for (i = first_lutv; i <= last_lutv; i++) {
        red = (short) round(r);                            /* round out */
        gre = (short) round(g);                            /* given r g */
        blu = (short) round(b);                            /* b value   */
        mapcolor(i, red, gre, blu);      /* assign next color map index */
        r += rdx;                                          /* increment */
        g += gdx;                                          /* color in- */
        b += bdx;                                          /* dices     */
    }    
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
    rotate(-6, 'y');           
    rotate(4, 'z');          

    multmatrix(objmat);
    getmatrix(objmat);

    popmatrix();

}


draw_scene() {

    czclear(BACKGROUND, zfar);

    perspective(400, scrnaspect, 30.0, 60.0);
    translate(0.0, 0.0, -40.0);
    multmatrix(objmat);
    rotate(-580, 'y');                /* to skew original view to show both */
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
    color(STARTINDEX+32);
    v3f(&polygon1[0][0]);
    color(STARTINDEX+63);
    v3f(&polygon1[1][0]);
    color(STARTINDEX+48);
    v3f(&polygon1[2][0]);
    endpolygon();

    bgnpolygon();
    color(ENDINDEX);
    v3f(&polygon2[0][0]);
    color(STARTINDEX+80);
    v3f(&polygon2[1][0]);
    color(STARTINDEX+64);
    v3f(&polygon2[2][0]);
    endpolygon();

    bgnpolygon();
    color(STARTINDEX);
    v3f(&polygon3[0][0]);
    color(STARTINDEX+16);
    v3f(&polygon3[1][0]);
    color(STARTINDEX+31);
    v3f(&polygon3[2][0]);
    color(STARTINDEX+16);
    v3f(&polygon3[3][0]);
    endpolygon();
}
