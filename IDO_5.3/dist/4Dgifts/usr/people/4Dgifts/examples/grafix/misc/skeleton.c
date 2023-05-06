/*
 *  skeleton.c
 *
 *    This program demonstrates an approach for catching and processing
 *    input from the GL event queue.  This approach has the advantage
 *    that it does not eat up CPU cycles in polling loops.  Past examples 
 *    have included very expensive qtest() or getbutton() cycling that 
 *    degrade the performance of other applications even when not
 *    in motion.
 *
 *    At present ~4Dgifts/examples/grafix/{zrgb, zrgbmenu, zcmapmenu}
 *    as well as ~4Dgifts/examples/nurbs/model are all examples which 
 *    employ this input processing technique derived from this template.
 *
 *    This example does not address issues brought up by programs that
 *    require the use of multiple windows.  However, it should serve as
 *    an adequate starting point.
 *  
 *                                 Thant Tessman - 1990
 */
#include <gl/gl.h>
#include <gl/device.h>

/*
 * external object orientation & identity matrix declarations/initializations
 */
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


enum {NOTHING, ORIENT} mode;
/*
    Other possible modes could be things like TRANSLATE.
    Add different cases to the switch statement in update_scene()
*/


int omx=0, mx=0, omy=0, my=0;                 /* old and new mouse position */

main() {

    long dev;
    short val;
    int redrawneeded=TRUE;        /* Is true when the scene needs redrawing */


    initialize();               /* set up graphics and initialize variables */

    while (TRUE) {                            /* main input processing loop */

        /*
         * First, if we need to redraw the scene for any reason, redraw
         * it.  
         */
        if (redrawneeded) {
            draw_scene();

            /*
             * There could be a complicated conditional here to decide if
             * the program needs to constantly redraw itself or not, based
             * on (perhaps) whether or not anything in the scene is
             * moving.  In this simple example, the program is entirely
             * event-driven.
             */
            redrawneeded=FALSE;
        }

      /*
       * The program will stay in the loop below as long as there are
       * events to be processed; as long as there are no lengthy
       * operations inside the loop, there is no danger that events will
       * occur faster than it can process them.
       *
       * Also, if the scene doesn't need redrawing, the loop will also
       * be entered, and the program will block in qread() until an
       * event occurs.
       */
        while (qtest() || (!redrawneeded)) {

            switch(dev=qread(&val)) {

                case ESCKEY:
                    if (val)                        /* exit on up, not down */
                        break;
                    exit(0);

                case REDRAW:
                    reshapeviewport();
                    redrawneeded = 1;
                    break;

                case MIDDLEMOUSE:
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

            }    /* End of switch */
        }            /* End of input gathering loop */
    }                    /* End of main (infinite) loop */
}


initialize() {                   /* initialize the graphics subsystem */

    keepaspect(5, 4);
    winopen("skeleton");

    doublebuffer();
    cmode();
    gconfig();

    qdevice(ESCKEY);
    qdevice(MIDDLEMOUSE);
    qdevice(LEFTMOUSE);
    qdevice(MOUSEX);
    qdevice(MOUSEY);
}


/*
 *              update_scene
 *
 *    do whatever computation is required by the current mode
 */
update_scene() {

    switch (mode) {

        case ORIENT:
            orient();
            break;
    }
}


/*
 *              orient
 *
 *      perform post-multiplication on the object's orientation matrix
 *
 *      orient "fools" the pre-multiply matrix system used in the graphics
 *      pipeline by saving the current viewing projection, then loading an
 *      identiy matrix onto the stack, multiplying into this identity matrix
 *      the new rotation angles in x and y, multiplying these newest
 *      rotations into the object orientation matrix, then immediately
 *      saving out the cummulatively altered new matrix (for the next time),
 *      and finally restoring the current viewing projection.
 */
orient () {

    pushmatrix();

    loadmatrix(idmat);

    rotate(mx-omx, 'y');
    rotate(omy-my, 'x');

    multmatrix(objmat);
    getmatrix(objmat);

    popmatrix();
}


/*
 *              draw_scene
 *
 *      draws the next frame of the current model being displayed
 */
draw_scene() {

    color(BLACK);
    clear();

    perspective(400, 5.0/4.0, 2.0, 6.0);
    translate(0.0, 0.0, -4.0);
    multmatrix(objmat);
    draw_cube();

    swapbuffers();
}


/*
 *   define the model being used and the routine that draws it
 */
float cube_vert[8][3] = {

    { 1.0,  1.0, -1.0,},
    { 1.0, -1.0, -1.0,},
    {-1.0, -1.0, -1.0,},
    {-1.0,  1.0, -1.0,},
    { 1.0,  1.0,  1.0,},
    { 1.0, -1.0,  1.0,},
    {-1.0, -1.0,  1.0,},
    {-1.0,  1.0,  1.0,},
};


draw_cube() {

    color(WHITE);

    bgnclosedline();
    v3f(cube_vert[0]);
    v3f(cube_vert[1]);
    v3f(cube_vert[2]);
    v3f(cube_vert[3]);
    endclosedline();

    bgnclosedline();
    v3f(cube_vert[4]);
    v3f(cube_vert[5]);
    v3f(cube_vert[6]);
    v3f(cube_vert[7]);
    endclosedline();

    bgnline();
    v3f(cube_vert[0]);
    v3f(cube_vert[4]);
    endline();

    bgnline();
    v3f(cube_vert[1]);
    v3f(cube_vert[5]);
    endline();

    bgnline();
    v3f(cube_vert[2]);
    v3f(cube_vert[6]);
    endline();

    bgnline();
    v3f(cube_vert[3]);
    v3f(cube_vert[7]);
    endline();
}
