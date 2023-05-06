/* 
 *  draw.c:
 *
 *              an absolutely  minimal line drawing program.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

float xmax,ymax;
long xorig, yorig, xsize, ysize;

main()
{
    Device dev;
    short val;
    short xpos, ypos;

    initialize();

    while(TRUE) {
	dev = qread(&val);
	switch(dev) {	        /* wait for mouse down */
	    case ESCKEY:	/* quit */
		gexit();
		exit(0);
	    case REDRAW:
		reshapeviewport();
                getorigin(&xorig, &yorig);
                ortho2(xorig-0.5, xorig+xsize-0.5, yorig-0.5, yorig+ysize-0.5);
		color(BLACK);
		clear();
		color(RED);
		break;
	    case MIDDLEMOUSE:	/* move */
		qread(&xpos);
		qread(&ypos);
		move2i((long) xpos, (long) ypos);
		qread(&val);	/* these three reads clear out */
		qread(&val);	/* the queue */
		qread(&val);
		break;
	    case LEFTMOUSE:	/* draw */
		qread(&xpos);
		qread(&ypos);
		draw2i((long) xpos, (long) ypos);
		qread(&val);
		qread(&val);
		qread(&val);
		break;
	    default:
		break;
	}
    }
}

initialize()
{
    long xmaxscrn, ymaxscrn;     /* maximum size of screen in x and y       */


    xmaxscrn = getgdesc(GD_XPMAX)-1;
    ymaxscrn = getgdesc(GD_YPMAX)-1;
    prefposition(xmaxscrn/4,xmaxscrn*3/4,ymaxscrn/4,ymaxscrn*3/4);
    winopen("draw");
    getorigin(&xorig, &yorig);
    getsize(&xsize, &ysize);
 
    xmax = .5 + (float) xmaxscrn;
    ymax = .5 + (float) ymaxscrn;
    ortho2(xmax/4.0,xmax*3.0/4.0,ymax/4.0,ymax*3.0/4.0);
 
    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
    qdevice(MIDDLEMOUSE);
    tie(LEFTMOUSE, MOUSEX, MOUSEY);
    tie(MIDDLEMOUSE, MOUSEX, MOUSEY);
  
    color(BLACK);
    clear();
    color(RED);
}
