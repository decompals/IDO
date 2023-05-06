/*
 *  select1.c:
 *
 *     gselect demo program.  The "ship" is the blue rectangle.  The "planet" 
 *     is the green circle.  Move the ship using LEFTMOUSE so it intersects 
 *     the planet, the ship will crash, and you will hear the "bell of agony".
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

#define X		0
#define Y		1
#define XY		2

#define	WSIZE		400 
#define BUFSIZE		10
#define PLANET		109
#define SHIPWIDTH	20
#define SHIPHEIGHT	10

void initialize();
void drawscene();
void drawplanet();

main()
{
    float ship[XY];
    long org[XY];
    long size[XY];
    Device dev;
    short val;
    Device mdev[XY];
    short mval[XY];
    long nhits;
    short buffer[BUFSIZE];
    Boolean run;

    initialize();
    mdev[X] = MOUSEX;
    mdev[Y] = MOUSEY;

    run = TRUE;
    while (run) {
	dev = qread(&val);
	if (dev == REDRAW) {
	    getorigin(&org[X], &org[Y]);
	    getsize(&size[X], &size[Y]);
	    ortho2(-0.5, size[X] - 0.5, -0.5, size[Y] - 0.5);
	    reshapeviewport();
	    drawscene();
	} else if (val == 0) {				/* on upstroke */
	    switch (dev) {
	    case LEFTMOUSE:
		getdev(XY, mdev, mval);
		ship[X] = mval[X] - org[X];
		ship[Y] = mval[Y] - org[Y];
		color(BLUE);
		sbox(ship[X], ship[Y], 
		     ship[X] + SHIPWIDTH, ship[Y] + SHIPHEIGHT);

		/*
		 * specify the selecting region to be a box surrounding the
		 * rocket ship 
		 */
		ortho2(ship[X], ship[X] + SHIPWIDTH, 
		       ship[Y], ship[Y] + SHIPHEIGHT);

		initnames();
		gselect(buffer, BUFSIZE);
		    loadname(PLANET);
		    /* no actual drawing takes place */
		    drawplanet();
		nhits = endselect(buffer);

		/*
		 * restore the Projection matrix; NB. can't use push/popmatrix 
		 * since they only work for the ModelView matrix stack 
		 * when in MVIEWING mode
		 */
		ortho2(-0.5, size[X] - 0.5, -0.5, size[Y] - 0.5);

		/* 
		 * check to see if PLANET was selected; NB. nhits is NOT the
		 * number of buffer elements written
		 */
		if (nhits < 0) {
		    fprintf(stderr, "gselect buffer overflow\n");
		    run = FALSE;
		} 
		else if (nhits >= 1 && buffer[0] == 1 && buffer[1] == PLANET)
		    ringbell();
		break;

	    case ESCKEY:
		run = FALSE;
		break;
	    }
	}
    }
    gexit();
    return 0;
}


void initialize()
{
    long org[XY];
    short gid;

    /* center window on screen and don't allow resizing */
    org[X] = (getgdesc(GD_XPMAX) - WSIZE)/2;
    org[Y] = (getgdesc(GD_YPMAX) - WSIZE)/2;
    prefposition(org[X], org[X] + WSIZE - 1, org[Y], org[Y] + WSIZE - 1);
    gid = winopen("select1");
    mmode(MVIEWING);
    prefsize(WSIZE, WSIZE);
    winconstraints();

    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);

    qenter(REDRAW, gid);
}

void drawscene()
{
    color(BLACK);
    clear();
    drawplanet();
}

void drawplanet()
{
    color(GREEN);
    circfi(200, 200, 20);
}
