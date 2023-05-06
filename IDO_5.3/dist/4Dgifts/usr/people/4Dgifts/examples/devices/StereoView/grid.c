/*
 *                                grid.c
 *
 *     Program that draws a test pattern for stereo; the left eye's image
 *   is blue, and has the text 'left' to the left.  The right eye's image
 *   is red, and says 'right' to the right.  If stereo is working properly,
 *   the grids should overlap exactly, creating a purplish image through 
 *   both eyes, and there should be minimal leaking from one eye to the other.
 */

#include <string.h>

#include <gl/gl.h>
#include <gl/device.h>
#include <gl/get.h>

#include "stereo.h"

/*
 * If, for some reason, stereo.h and libstereo.a aren't available, the
 * following #define's may be used instead of the previous #include:
 * #define YMAXSTEREO 491
 * #define YOFFSET 532
 *
 * I am anticipating the definitions potentially changing in the
 * future (due to incompatible screen formats, for example), and want
 * to minimize the number of changes I'll have to make.
 */

void
draw_grid()
{
	int i;

	ortho2(0, XMAXSCREEN+0.5, 0, YMAXSTEREO+0.5);

	recti(0, 0, XMAXSCREEN, YMAXSTEREO);

	translate(XMAXSCREEN/2.0, YMAXSTEREO/2.0, 0.0);
	scale(2.0, 1.0, 1.0);

	for (i = -128; i<=128; i+=32)
	{
		move2i(i, -128); draw2i(i, 128);
	}

	for (i = -128; i<=128; i+=32)
	{
		move2i(-128, i); draw2i(128, i);
	}

	circ(0.0, 0.0, YMAXSTEREO/2.0);
	circ(0.0, 0.0, 8.0);
}

void
draw_test()
{
	viewport(0, XMAXSCREEN, 0, YMAXSCREEN);
	color(BLACK); clear();

	color(RED);
	viewport(0, XMAXSCREEN, 0, YMAXSTEREO);
	draw_grid();
	cmov2i(180, 0);
	charstr("right");

	color(BLUE);
	viewport(0, XMAXSCREEN, YOFFSET, YOFFSET+YMAXSTEREO);
	draw_grid();
	cmov2i(-200, 0);
	charstr("left");
}

main(int argc, char **argv)
{
	long dev; short val;

	prefposition(0, getgdesc(GD_XPMAX), 0, getgdesc(GD_YPMAX));

	{	/* Open window with name of executable */
		char *t;
		winopen((t=strrchr(argv[0], '/')) != NULL ? t+1 : argv[0]);
	}

	stereo_on();	/* From libstereo.a */
	/* If libstereo.a isn't available, use:
	 * if (getgdesc(GD_STEREO)) setmonitor(STR_RECT);
	 */

	qdevice(ESCKEY);        /* press "Esc" key to exit */
	qdevice(LEFTMOUSE);	/* Keep window manager from moving window */
	qdevice(MIDDLEMOUSE);	/* Keep window manager from moving window */
	qdevice(WINQUIT);

	draw_test();

	while (1) 
	{
		switch (dev=qread(&val))
		{
		case ESCKEY:
			if (val) break;
		case WINQUIT:
			stereo_off();
			/*
			 * If libstereo.a isn't around, use:
			 * if (getgdesc(GD_STEREO)) setmonitor(monitor);
			 */
			exit(0);
		case REDRAW:
			draw_test();
			break;
		}
	}
}
