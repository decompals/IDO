/*
 * 	grid -
 *		Draw a 10 by 10 grid. This is useful for aligning
 *	a camera to the display.
 *
 *				Paul Haeberli - 1986
 */
#include <gl/gl.h>
#include <gl/device.h>

int drawit();

main()
{
    short val;

    makewindow();
    drawit();
    while(1)
	switch(qread(&val)) {
	    case RIGHTMOUSE:
	    case KEYBD:
		gexit();
		exit(0);
		break;
	}
}

drawit()
{
    reshapeviewport();
    ortho2(0.0,1.0,0.0,1.0);
    pushmatrix();
    color(0);
    clear();
    color(7);
    drawgrid(10,10);
    popmatrix();
}

/*
 *	Make a grid.
 *
 */
drawgrid(xdivs,ydivs)
int xdivs, ydivs;
{
    int i;
    float x, y;

    for(i=1; i<xdivs; i++) {
	x = (float)i/xdivs;
	move2(x,0.0);
	draw2(x,1.0);
    }
    for(i=1; i<ydivs; i++) {
	y = (float)i/ydivs;
	move2(0.0,y);
	draw2(1.0,y);
    }
}


makewindow() 
{
    long xmaxscrn, ymaxscrn;         /* maximum size of screen in x and y    */

    xmaxscrn = getgdesc(GD_XPMAX) - 1;
    ymaxscrn = getgdesc(GD_YPMAX) - 1;

    noborder();
    prefposition(0, xmaxscrn, 0, ymaxscrn);
    winopen ("grid");
    qdevice(KEYBD);
    qdevice(RIGHTMOUSE);
}
