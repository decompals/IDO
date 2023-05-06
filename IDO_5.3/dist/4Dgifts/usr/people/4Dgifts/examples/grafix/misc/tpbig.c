/* 
 *  tpbig.c:
 *
 *    Basic program demonstrating, among other things, arcs, polygons,
 *    character strings, and use of a textport.  
 *
 *    NOTE: To be able to correctly use textports, YOU MUST CALL 
 *    foreground() BEFORE YOU CALL winopen().  
 */

#include <gl/gl.h>
#include <gl/device.h>

void initialize();
void drawscene();
void do_exit();

main()
{
    Device dev;
    short val;

    initialize();

    while (TRUE) {
	dev = qread(&val);
	switch (dev) {
	    case ESCKEY:
		if (val == 0) {
		    do_exit();
		    /* NOTREACHED */		    
		}
		break;

	    case WINQUIT:
		do_exit();
		/* NOTREACHED */		    

            case REDRAW:
		reshapeviewport();
		drawscene();
		break;
        }
    }
    /* NOTREACHED */
}

void initialize()
{
    long xmaxscrn, ymaxscrn;
    Screencoord left, rite, botm, topp;
    short gid;

    xmaxscrn = getgdesc(GD_XPMAX);
    ymaxscrn = getgdesc(GD_YPMAX);
    left = 0.54*xmaxscrn;
    rite = 0.86*xmaxscrn - 1;
    botm = 0.75*ymaxscrn;
    topp = 0.92*ymaxscrn - 1;

    foreground();
    prefposition(0, xmaxscrn, 0, ymaxscrn);
    gid = winopen("tpbig");
    textport(left, rite, botm, topp);

    qdevice(ESCKEY);
    qdevice(WINQUIT);
    qenter(REDRAW, gid);
}


void drawscene()
{
    long i, j;
    long ht, wd;
    char singlechar[2];
    static Icoord cone[][2] = { 100, 300,
				150, 100,
				200, 300 };


    /* Draw an ice-cream cone */

    color(WHITE);
    clear();
    color(YELLOW);
    polf2i(3, cone);	/* draw the ice-cream cone */
    color(GREEN);			/* first scoop is mint */
    arcfi(150, 300, 50, 0, 1800);	/* only half of it shows */
    color(RED);				/* second scoop is cherry */
    circf(150.0, 400.0, 50.0);
    color(BLACK);
    poly2i(3, cone);	/* outline the cone in black */

    /* 
     * Next, draw a few filled and unfilled arcs in the upper
     * left corner of the screen.
     */

    arcf(100.0, 650.0, 40.0, 450, 2700);
    arci(100, 500, 40, 450, 2700);

    arcfi(250, 650, 80, 2700, 450);
    arc(250.0, 500.0, 80.0, 2700, 450);

    /* 
     * Now, put up a series of filled and unfilled rectangles with
     * the names of their colors printed inside of them across the
     * rest of the top of the screen.
     */

    color(GREEN);
    recti(400, 600, 550, 700);
    cmov2i(420, 640);
    charstr("Green");

    color(RED);
    rectfi(600, 600, 800, 650);
    color(BLACK);
    cmov2(690.0, 620.0);
    charstr("Red");

    color(BLUE);
    rect(810.0, 700.0, 1000.0, 20.0);
    cmov2i(900, 300);
    charstr("Blue");

    /* Now draw some text with a ruler on top to measure it by. */

    /* First the ruler: */

    color(BLACK);

    move2i(300, 400);
    draw2i(650, 400);
    for (i = 300; i <= 650; i += 10) {
	move2i(i, 400);
	draw2i(i, 410);
    }

    /* Then some text: */

    cmov2i(300, 380);
    charstr("The first line is drawn ");
    charstr("in two parts.");

    cmov2i(300, 368);
    charstr("This line is only 12 pixels lower.");

    cmov2i(300, 354);
    charstr("Now move down 14 pixels ...");

    cmov2i(300, 338);
    charstr("And now down 16 ...");

    cmov2i(300, 320);
    charstr("Now 18 ...");

    cmov2i(300, 300);
    charstr("And finally, 20 pixels.");

    /*
     * Finally, show off the entire font.  The cmov2i() before each
     * character is necessary in case that character is not defined.
     */

    singlechar[1] = '\0';
    ht = getheight() + 4;
    wd = strwidth("n");
    for (i = 0; i < 4; i++)
	for (j = 0; j < 32; j++) {
	    cmov2i(300 + wd*j, 200 - ht*i);
	    singlechar[0] = 32*i + j;
	    charstr(singlechar);
	}

    for (i = 0; i < 4; i++) {
	cmov2i(300, 100 - ht*i);
	for (j = 0; j < 32; j++) {
	    singlechar[0] = 32*i + j;
	    charstr(singlechar);
	}
    }

    tpon();		/* always keep the textport on top */
}

void do_exit()
{
    textinit();        /* reset textport */
    gexit();
    exit(0);
    /* NOTREACHED */
}
