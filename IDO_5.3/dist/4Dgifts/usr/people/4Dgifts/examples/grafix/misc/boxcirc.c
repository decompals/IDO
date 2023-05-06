/*                                                                          
 * boxcirc.c:
 * 
 *    A simple example which draws a 2D box and circle.
 */                                                                      

#include <gl/gl.h>
#include <gl/device.h>

#define X		0
#define Y		1
#define XY		2

#define	WSIZE		500 

void initialize();
void drawboxcirc();

main() 
{
    Device dev;
    short val;

    initialize();

    while (TRUE) {
	dev = qread(&val);
	if (dev == ESCKEY && val == 0) {
	    gexit();
	    exit(0);
	    /* NOTREACHED */
	} else if (dev == REDRAW) {
	    reshapeviewport();
	    drawboxcirc();
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
    gid = winopen("boxcirc");
    prefsize(WSIZE, WSIZE);
    winconstraints();

    qdevice(ESCKEY);
    qenter(REDRAW, gid);
}

void drawboxcirc() 
{
    pushmatrix();
    translate(200.0, 200.0, 0.0);
    color(BLACK);
    clear();
    color(BLUE);
    recti(0, 0, 100, 100);
    color(RED);
    circi(50, 50, 50);
    popmatrix();
}
