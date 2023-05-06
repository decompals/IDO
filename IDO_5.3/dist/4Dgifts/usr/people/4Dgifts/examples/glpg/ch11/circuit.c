#include <gl/gl.h>
#include <gl/device.h>

#define R		0
#define G		1
#define B		2
#define RGB		3

#define BACKGROUND      0x0	/* = 00 */
#define POWER           0x1	/* = 01 */
#define GROUND          0x2	/* = 10 */
#define SHORT           0x3	/* = 11 */

void powerrect(x1, y1, x2, y2)
Icoord x1, y1, x2, y2;
{
    writemask(0x1);
    color(POWER);
    sboxfi(x1, y1, x2, y2);
}

void groundrect(x1, y1, x2, y2)
Icoord x1, y1, x2, y2;
{
    writemask(0x2);
    color(GROUND);
    sboxfi(x1, y1, x2, y2);
}

void clearcircuit()
{
    writemask(0x3);
    color(BACKGROUND);
    clear();
}

main()
{
    int i, drawtype;
    Device dev;
    short val;
    short x1, y1, x2, y2;
    long xorg, yorg;
    Boolean run;
    short saved[SHORT+1][RGB];

    prefsize(400, 400);
    winopen("circuit");
    color(BLACK);
    clear();
    qdevice(PKEY);			/* draw power rectangles */
    qdevice(GKEY);			/* draw ground rectangles */
    qdevice(CKEY);			/* clear screen */
    qdevice(ESCKEY);			/* quit */
    qdevice(WINQUIT);			/* quit from window manager */
    qdevice(LEFTMOUSE);			/* mark rectangle corners */
    tie(LEFTMOUSE, MOUSEX, MOUSEY);
    getorigin(&xorg, &yorg);

    /* save existing color map */
    for (i = BACKGROUND; i <= SHORT; i++)
	getmcolor(i, &saved[i][R], &saved[i][G], &saved[i][B]);

    /* load new color map */
    mapcolor(BACKGROUND, 0, 0, 0);	/* black */
    mapcolor(POWER, 0, 0, 255);		/* blue */
    mapcolor(GROUND, 255, 255, 255);	/* white */
    mapcolor(SHORT, 255, 0, 0);		/* red */

    drawtype = GROUND;
    run = TRUE;
    while (run) {
	dev = qread(&val);
	if (dev == WINQUIT)
	    run = FALSE;
	else if (dev == LEFTMOUSE) {	/* downclick */
	    qread(&x1);
	    qread(&y1);
	    qread(&val);		/* upclick */
	    qread(&x2);
	    qread(&y2);
	    if (drawtype == POWER)
		powerrect(x1 - xorg, y1 - yorg, x2 - xorg, y2 - yorg);
	    else
		groundrect(x1 - xorg, y1 - yorg, x2 - xorg, y2 - yorg);
	}
	else if (val == 0) {		/* on upstroke only */
	    switch (dev) {
	    case PKEY:
		drawtype = POWER;
		break;
	    case GKEY:
		drawtype = GROUND;
		break;
	    case CKEY:
		clearcircuit();
		break;
	    case ESCKEY:
		run = FALSE;
		break;
	    }
	}
    }

    /* restore default color map */
    for (i = BACKGROUND; i <= SHORT; i++)
	mapcolor(i, saved[i][R], saved[i][G], saved[i][B]);
    gexit();
    return 0;
}
