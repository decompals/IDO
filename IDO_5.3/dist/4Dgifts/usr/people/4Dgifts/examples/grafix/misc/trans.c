/* 
 *  trans.c
 *
 *    uses the home-made popup menus (like in popup.c) in a program that
 *    lets the user do modeling transformations on a wireframed cube
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>
#include "popup.h"

#define TRANSX 1
#define TRANSY 2
#define TRANSZ 3
#define ROTX 4
#define ROTY 5
#define ROTZ 6
#define SCALEX 7
#define SCALEY 8
#define SCALEZ 9
#define EXITTRANS 10

popupentry mainmenu[] = {
    TRANSX, "Translate X",
    TRANSY, "Translate Y",
    TRANSZ, "Translate Z",
    ROTX, "Rotate X",
    ROTY, "Rotate Y",
    ROTZ, "Rotate Z",
    SCALEX, "Scale X",
    SCALEY, "Scale Y",
    SCALEZ, "Scale Z",
    EXITTRANS, "Quit",
    0, 0
};

long barwin, cubewin;

main()
{
    short val;
    int command, i;
    float transparam;
    float scrnaspect;            /* aspect ratio value */



    foreground();
    xmaxscrn = getgdesc(GD_XPMAX)-1;
    ymaxscrn = getgdesc(GD_YPMAX)-1;
    scrnaspect = ((float)xmaxscrn)/ymaxscrn;
    prefposition(0, xmaxscrn, 0, ymaxscrn-20);
    cubewin = winopen("trans");

    perspective(400, scrnaspect, 0.1, 1000.0);
    lookat(10.0, 10.0, 10.0, 0.0, 0.0, 0.0, 0);

    doublebuffer();
    gconfig();

    initpopup();
    color(BLACK);
    clear();
    drawcube();
    drawaxes();
    swapbuffers();

    qdevice(RIGHTMOUSE);
    qdevice(ESCKEY);
    qdevice(WINQUIT);

    prefposition(10, xmaxscrn-10, 10, 100);
    barwin = winopen("trans");
    wintitle("bar window");
    drawbar(-10.0, 10.0);

    while (1) {
	switch(qread(&val)) {
            case ESCKEY:
            case WINQUIT:
                greset();
                gexit();
                exit(0);
		break;
            case LEFTMOUSE:
	        switch (command = popup(mainmenu)) {
	            case 0:
		        continue;
	            case TRANSX:
	            case TRANSY:
	            case TRANSZ:
	            case SCALEX:
	            case SCALEY:
	            case SCALEZ:
		        drawbar(-10.0, 10.0);
		        break;
	            case ROTX:
	            case ROTY:
	            case ROTZ:
		        drawbar(0.0, 3600.0);
		        break;
	            case EXITTRANS:
		        greset();
		        gexit();
		        exit(0);
	        }
		break;

            default:
		break;
        }
	while (qtest() == 0)	/* while no buttons pressed */ {
	    if (readbar(&transparam)) {
		color(BLACK);
		clear();
		drawaxes();
		pushmatrix();
		switch (command) {
		    case TRANSX:
			translate(transparam, 0.0, 0.0);
			break;
		    case TRANSY:
			translate(0.0, transparam, 0.0);
			break;
		    case TRANSZ:
			translate(0.0, 0.0, transparam);
			break;
		    case SCALEX:
			scale(transparam, 1.0, 1.0);
			break;
		    case SCALEY:
			scale(1.0, transparam, 1.0);
			break;
		    case SCALEZ:
			scale(1.0, 1.0, transparam);
			break;
		    case ROTX:
			rotate((int)transparam, 'x');
			break;
		    case ROTY:
			rotate((int)transparam, 'y');
			break;
		    case ROTZ:
			rotate((int)transparam, 'z');
			break;
		}
		drawcube();
		popmatrix();
		swapbuffers();
	    }
	}
    }
}

float barmin, barmax, bardelta;
/*
*/

drawbar(minval, maxval)
float minval, maxval;
{
    register i;
    char str[20];
    float winleng = xmaxscrn - 200.0;
    long sliderincr = winleng / 4;

    winset(barwin);
    color(BLACK);
    clear();
    barmin = minval;
    barmax = maxval;
    bardelta = (barmax - barmin)/winleng;
    cursoff();
    color(RED);
    recti(100, 20, xmaxscrn-100, 40);
    for (i = 0; i < 5; i++) {
	move2i(100 + i*sliderincr, 40);
	draw2i(100 + i*sliderincr, 50);
	cmov2i(103 + i*sliderincr, 44);
	sprintf(str, "%6.2f", minval + i*(maxval - minval)/4.0);
	charstr(str);
    }
    curson();
    swapbuffers();
    winset(cubewin);
}

/* The readbar routine returns 1 if the value stored in retval is valid,
 * and zero otherwise.
 */

readbar(retval)
float *retval;
{
    int xmouse, ymouse;

    ymouse = getvaluator(MOUSEY) - 10;
    if (10 <= ymouse && ymouse <= 80) {
	xmouse = getvaluator(MOUSEX) - 110;
	if (100 <= xmouse && xmouse <= xmaxscrn-100) {
	    *retval = barmin + bardelta * xmouse;
	    return 1;
	}
    }
    return 0;
}

drawcube()
{

	color(RED);

	/* First draw the outline of the cube */

	move(-1.0, -1.0, -1.0);
	draw(1.0, -1.0, -1.0);
	draw(1.0, 1.0, -1.0);
	draw(-1.0, 1.0, -1.0);
	draw(-1.0, -1.0, -1.0);
	draw(-1.0, -1.0, 1.0);
	draw(1.0, -1.0, 1.0);
	draw(1.0, 1.0, 1.0);
	draw(-1.0, 1.0, 1.0);
	draw(-1.0, -1.0, 1.0);
	move(-1.0, 1.0, -1.0);
	draw(-1.0, 1.0, 1.0);
	move(1.0, 1.0, -1.0);
	draw(1.0, 1.0, 1.0);
	move(1.0, -1.0, 1.0);
	draw(1.0, -1.0, -1.0);

	/* now draw the letters 'X', 'Y', and 'Z' on the faces: */

	move(1.0, -0.6666666, -0.5);
	draw(1.0, 0.6666666, 0.5);
	move(1.0, 0.6666666, -0.5);
	draw(1.0, -0.6666666, 0.5);

	move(0.0, 1.0, 0.6666666);
	draw(0.0, 1.0, 0.0);
	draw(0.5, 1.0, -0.6666666);
	move(0.0, 1.0, 0.0);
	draw(-0.5, 1.0, -0.6666666);

	move(-0.5, 0.6666666, 1.0);
	draw(0.5, 0.6666666, 1.0);
	draw(-0.5, -0.6666666, 1.0);
	draw(0.5, -0.6666666, 1.0);
	closeobj();
}

drawaxes()
{
	color(YELLOW);
	movei(0, 0, 0);
	drawi(0, 0, 2);
	movei(0, 2, 0);
	drawi(0, 0, 0);
	drawi(2, 0, 0);
	cmovi(0, 0, 2);
	charstr("z");
	cmovi(0, 2, 0);
	charstr("y");
	cmovi(2, 0, 0);
	charstr("x");
	closeobj();
}
