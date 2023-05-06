/*      
 *   overlay_cursor.c:
 *
 *   As well as demonstarting usage of the overlay bitplanes, a fly-shaped 
 *   cursor is also included.
 *   Handling cursors on the 4D is very different from the IRIS 3000.
 *   There is a CURSORDRAW mode for creating cursor colors.  
 *   The default cursor type is single colored, 16 by 16 pixels.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

static unsigned short fly[16] = { 0x0000, 0x1818, 0x2574, 0x2244,
                                  0x2184, 0xA185, 0xA185, 0xA185,
                                  0x518A, 0x2994, 0x1FF8, 0x15A8,
                                  0x2A54, 0x4BD2, 0x4422, 0x4002 };
Boolean MACHMODE;

main () {
    int   oldx, oldy, curx, cury, dx, dy;
    long  xorig, yorig;     /* used to keep track of the location and size */
    long  xsize, ysize;    /* of the current window for overlay maintenance */
    int   attached;
    int   dev;
    short val;

    initialize ();
    attached = 1;
    getorigin(&xorig, &yorig);
    getsize(&xsize, &ysize);
    curx = xsize;
    cury = ysize;
    dx = xsize / 2;
    dy = ysize / 2;

    while (TRUE) {
        while (qtest () || !attached) {
            dev = qread (&val);
            switch (dev) {
                case WINFREEZE:                  /*  window is iconized */
                    break;
                case REDRAWICONIC:       /*  window as an icon is moved */
		    frontbuffer(TRUE);
		    pushmatrix();
		    reshapeviewport();
                    drawscene (xsize/2, ysize/2);
		    popmatrix();
		    frontbuffer(FALSE);
                    break;
                case REDRAW:            /* window is moved or reshaped  */
                    getorigin(&xorig, &yorig);
                    reshapeviewport ();
                    drawhouse ();
                    drawscene (dx, dy);
                    swapbuffers ();
                    break;
                case INPUTCHANGE: 
                    attached = (int) val;
                    break;
                case WINQUIT:
                case WINSHUT:
                case ESCKEY: 
                    gexit ();
                    exit (0);
                    break;
                default: 
                    break;
            }
        }
        if (attached != 0) {
            oldx = curx;
            oldy = cury;
            curx = getvaluator (MOUSEX);
            cury = getvaluator (MOUSEY);
            if (getbutton (LEFTMOUSE)) {
                dx = dx + (curx - oldx);
                dy = dy + (cury - oldy);
            }
            drawscene (dx, dy);
            swapbuffers ();
        }
    }
}

/*  A cursor in the shape of a fly is created here.  On the 4D, 
 *  the user has control over the color of the cursor.  
 */
initialize () {
    long xmaxscrn, ymaxscrn;     /* maximum size of screen in x and y       */
    Colorindex dummy;            /* for strict ANSI C prototyping           */


    xmaxscrn = getgdesc(GD_XPMAX)-1;
    ymaxscrn = getgdesc(GD_YPMAX)-1;
    prefposition ((xmaxscrn - 750) / 2, (xmaxscrn + 750) / 2,
                  (ymaxscrn - 600) / 2, (ymaxscrn + 600) / 2);
    iconsize(86,66);
    winopen ("car");
    minsize (750, 600);
    doublebuffer ();
    gconfig ();
    shademodel (FLAT);
    qdevice (ESCKEY);
    qdevice (WINFREEZE);
    qdevice (WINTHAW);
    qdevice (WINSHUT);
    qdevice (WINQUIT);

    qenter (REDRAW, 0);

    if (getgdesc(GD_BITS_OVER_SNG_CMODE) < 2) /* test to see if we're on an */
	MACHMODE = PUPDRAW;                   /* 8-bit PI or Hollywood mach */
    else {
	MACHMODE = OVERDRAW;
	overlay(2);                             /* define/setup overlays */
	gconfig();
    }

    /*  there is no frontbuffer or backbuffer for overlay  */
    drawmode (MACHMODE);
    mapcolor (1, 255, 0, 255);
    mapcolor (2, 0, 255, 255);
    drawmode (NORMALDRAW);
    drawhouse ();

/*  make a blue cursor in the shape of a fly.  Use defcursor() to load the 
 *  cursor table.   Load  the blue  RGB into the cursor color map.  On the 
 *  3000,  to  activate  the  cursor,   setcursor()  is  called with three 
 *  parameters:  cursor table index, color, and writemask. The curorigin() 
 *  routine makes the  "hot spot"  (where the valuator is read  from)  the 
 *  middle of the cursor. On the 4D, setcursor() only takes one parameter, 
 *  the  entry  in  the  cursor table.   The cursor color has already been 
 *  defined, and the 4D cursor has no writemask.
 */
    drawmode (CURSORDRAW);
    defcursor (1, fly);
    curorigin (1, 8, 8);
    mapcolor (1, 0, 0, 255);
    setcursor (1, dummy, dummy);
    drawmode (NORMALDRAW);
}


/*  Everytime through the loop, draw only the car, not the house.  */
drawscene (x, y)
int     x, y;
{
    color (BLACK);
    clear ();
    drawcar (x, y);
}

/*  draw a car with several colors.
 *  The car itself is drawn with  the front window first.  Then the front
 *  window is flipped over (scaled) for the  rear  window.  The translate
 *  routine moves the car to an (x,y) position.
 */
drawcar (x, y)
int     x, y;
{
    float   fx, fy;

    fx = (float) x;
    fy = (float) y;
    pushmatrix ();
    translate (fx, fy, 0.0);        /*  move to mouse location  */
    color (BLUE);                   /*  wheels  */
    circfi (-75, -75, 20);
    circfi (75, -75, 20);
    color (RED);                    /*  car body  */
    pmv2i (-150, -50);
    pdr2i (-125, 0);
    pdr2i (125, 0);
    pdr2i (150, -50);
    pclos ();
    color (YELLOW);                 /*  front window  */
    drawwindow ();
    color (GREEN);                  /*  rear window  */
    scale (-1.0, 1.0, 1.0);
    drawwindow ();
    popmatrix ();
}

/*  draw a window for the car  */
drawwindow () {
    pmv2i (0, 0);
    pdr2i (0, 50);
    pdr2i (50, 50);
    pdr2i (75, 0);
    pclos ();
}

/*  draw a house in two colors at a fixed position, specified by the trans-
 *  late routine.  The house is drawn with colors in the 4D overlay bitplanes.
 */
drawhouse () {
    drawmode (MACHMODE);
    color (0);
    clear ();
    pushmatrix ();
    translate (200.0, 100.0, 0.0);     /*  move house into position  */
    color (1);                         /*  roof   */
    pmv2i (0, 0);
    pdr2i (0, 250);
    pdr2i (350, 250);
    pdr2i (350, 0);
    pclos ();
    color (2);                         /*  1st floor  */
    pmv2i (175, 400);
    pdr2i (0, 250);
    pdr2i (350, 250);
    pclos ();
    popmatrix ();
    drawmode (NORMALDRAW);
}
