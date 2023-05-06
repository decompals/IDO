/*
 *   backface.c:
 *
 *     Draw a cube that can run with backface() turned on or off.
 *     The cube is moved when LEFTMOUSE is pressed and mouse itself is moved.
 */

#include <gl/gl.h>
#include <gl/device.h>

#define X		0
#define Y		1
#define XY		2

#define	WSIZE		600 
#define CUBE_SIZE	200.0

void initialize();
void drawcube();
void drawscene();

main () 
{
    Device dev;
    short val, x = 30, y = 30;
    Boolean moveit = FALSE;

    initialize();

    while (TRUE) {
        while (qtest() || !moveit) {
            dev = qread(&val);
            if (dev == ESCKEY && val == 0) {
                gexit();
                exit(0);
		/* NOTREACHED */
            } else if (dev == REDRAW) {
                reshapeviewport();
                drawscene(x, y);
            } else if (dev == LEFTMOUSE) {
		/* move cube while left mouse button is down */
                moveit = val;                 
            } else if (dev == BKEY && val == 0) {
		/* on upstroke of key, turn back-facing off */
                backface(TRUE);               
                drawscene(x, y);
            } else if (dev == FKEY && val == 0) {
		/* on upstroke of key, turn back-facing on */
                backface(FALSE);              
                drawscene(x, y);
            }
        }
        if (moveit) {
            x = getvaluator(MOUSEX);
            y = getvaluator(MOUSEY);
            drawscene(x, y);
        } 
    }
    /* NOTREACHED */
}

void initialize() 
{
    long org[XY];
    short gid;

    /* center window on screen and don't allow resizing */
    org[X] = (getgdesc(GD_XPMAX) - WSIZE)/2;
    org[Y] = (getgdesc(GD_YPMAX) - WSIZE)/2;
    prefposition(org[X], org[X] + WSIZE - 1, org[Y], org[Y] + WSIZE - 1);
    gid = winopen("backface");
    prefsize(WSIZE, WSIZE);
    winconstraints();

    doublebuffer();
    gconfig();
    shademodel(FLAT);
    backface(TRUE);             /* turn on back-facing polygon removal */

    ortho((float)-WSIZE, (float)WSIZE, 
          (float)-WSIZE, (float)WSIZE,
          (float)-WSIZE, (float)WSIZE);

    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
    qdevice(BKEY);
    qdevice(FKEY);
    qenter(REDRAW, gid);
}

/* draw a cube */
void drawcube() 
{
    /* front face */
    pushmatrix();
    translate(0.0, 0.0, CUBE_SIZE);
    color(RED);
    rectf(-CUBE_SIZE, -CUBE_SIZE, CUBE_SIZE, CUBE_SIZE);
    popmatrix();

    /* right face */
    pushmatrix();
    translate(CUBE_SIZE, 0.0, 0.0);
    rotate(900, 'y');
    color(GREEN);
    rectf(-CUBE_SIZE, -CUBE_SIZE, CUBE_SIZE, CUBE_SIZE);
    popmatrix();

    /* back face */
    pushmatrix();
    translate(0.0, 0.0, -CUBE_SIZE);
    rotate(1800, 'y');
    color(BLUE);
    rectf(-CUBE_SIZE, -CUBE_SIZE, CUBE_SIZE, CUBE_SIZE);
    popmatrix();

    /* left face */
    pushmatrix();
    translate(-CUBE_SIZE, 0.0, 0.0);
    rotate(-900, 'y');
    color(CYAN);
    rectf(-CUBE_SIZE, -CUBE_SIZE, CUBE_SIZE, CUBE_SIZE);
    popmatrix();

    /* top face */
    pushmatrix();
    translate(0.0, CUBE_SIZE, 0.0);
    rotate(-900, 'x');
    color(MAGENTA);
    rectf(-CUBE_SIZE, -CUBE_SIZE, CUBE_SIZE, CUBE_SIZE);
    popmatrix();

    /* bottom face */
    pushmatrix();
    translate(0.0, -CUBE_SIZE, 0.0);
    rotate(900, 'x');
    color(YELLOW);
    rectf(-CUBE_SIZE, -CUBE_SIZE, CUBE_SIZE, CUBE_SIZE);
    popmatrix();
}

void drawscene(x, y)
short x, y;
{
    pushmatrix();
    rotate(2*x, 'y');
    rotate(2*y, 'x');
    color(BLACK);
    clear();
    drawcube();        
    popmatrix();
    swapbuffers();
}
