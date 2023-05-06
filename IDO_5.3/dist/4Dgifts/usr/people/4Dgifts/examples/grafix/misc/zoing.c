/*
 *	zoing - 
 *		Make a kind of wacky spiral out of circles.
 *
 *				Paul Haeberli - 1984
 *
 */

#include <gl/gl.h>
#include <gl/device.h>

void initialize();
void drawscene();

main()
{
    short val;
    Device dev;

    initialize();

    while (TRUE) {
	dev = qread(&val);
	switch (dev) {
	    case ESCKEY:
		if (val == 0) {
		    gexit();
		    exit(0);
		    /* NOTREACHED */
		}
		break;

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
    short gid;

    keepaspect(1, 1);
    gid = winopen("zoing");

    qdevice(ESCKEY);
    qenter(REDRAW, gid);
}

void drawscene()
{
    int i;

    color(WHITE);
    clear();
    color(BLACK);
    ortho2(-1.0, 1.0, -1.0, 1.0);
    translate(-0.1, 0.0, 0.0);
    pushmatrix();
    for (i = 0; i < 200; i++)  {
	rotate(170, 'z');
	scale(0.96, 0.96, 0.0);
	pushmatrix();
	    translate(0.10, 0.0, 0.0);
	    circ(0.0, 0.0, 1.0);
	popmatrix();
    }
    popmatrix();
}
