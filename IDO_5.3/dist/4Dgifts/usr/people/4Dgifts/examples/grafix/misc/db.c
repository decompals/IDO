/*
 *   db.c:
 *
 *    A double buffered window manager program.  Draws a cube which 
 *    is rotated by movements of the mouse.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>

main()
{
    int moveit, x, y;	/*  current rotation of object	*/
    Device dev;
    short val;


    x = 0;  
    y = 0;
    initialize();

    while(TRUE) {

        while (qtest()) {	/*  process queued tokens */

            dev = qread(&val);

	    switch(dev) {
	         case ESCKEY:	/*  exit program with ESC */
		     exit(0);
		     break;
		  case REDRAW:
		     reshapeviewport();
		     drawcube(x,y);
		     break;
		  default:
		     break;
	    }
        }
	x = getvaluator(MOUSEX);
	y = getvaluator(MOUSEY);
	drawcube(x,y);
    }
}

initialize()
{
    int gid;
    long xmaxscrn, ymaxscrn;           /* maximum size of screen in x and y */

    xmaxscrn = getgdesc(GD_XPMAX)-1;
    ymaxscrn = getgdesc(GD_YPMAX)-1;
    prefposition(xmaxscrn/4,xmaxscrn*3/4,ymaxscrn/4,ymaxscrn*3/4);
    gid = winopen("db");
    winconstraints();

    doublebuffer();
    gconfig();
    shademodel(FLAT);

    qdevice(ESCKEY);
    qenter(REDRAW,gid);

    perspective(400, 3.0/2.0, 0.001, 100000.0);
    translate(0.0, 0.0, -3.0);
}

drawcube(rotx,roty)
int rotx, roty;
{
    color(BLACK);
    clear();
    color(WHITE);
    pushmatrix();
    rotate(rotx,'x');
    rotate(roty,'y');
    cube();
    scale(0.3,0.3,0.3);
    cube();
    popmatrix();
    swapbuffers();
}

cube() /* make a cube out of 4 squares */
{
    pushmatrix();
    side();
    rotate(900,'x');
    side();
    rotate(900,'x');
    side();
    rotate(900,'x');
    side();
    popmatrix();
}

side() /* make a square translated 0.5 in the z direction */
{
    pushmatrix();
    translate(0.0,0.0,0.5);
    rect(-0.5,-0.5,0.5,0.5);
    popmatrix();
}
