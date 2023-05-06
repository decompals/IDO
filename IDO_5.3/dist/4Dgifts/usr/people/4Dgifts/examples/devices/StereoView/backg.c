/*
 *	texback - 
 *		make a two colored textured background.
 *
 *				Paul Haeberli - 1984
 *
 *      Modified by Thant Tessman, then Gavin Bell to do stereo, 1990
 */
#include <stdio.h>
#include <string.h>

#include <gl/gl.h>
#include <gl/device.h>
#include <gl/get.h>
#include "stereo.h"

#define SEP 0

/*
 * Local function prototypes
 */
void background();
void draw_pattern();

main(argc,argv)
int argc;
char **argv;
{
	int dev;
	short val;

	prefposition(0, getgdesc(GD_XPMAX), 0, getgdesc(GD_YPMAX));

	{	/* Open window with name of executable */
		char *t;
		winopen((t=strrchr(argv[0], '/')) != NULL ? t+1 : argv[0]);
	}

	stereo_on();	/* From libstereo.a */

	qdevice(ESCKEY);	/* Need to turn off stereo on quit */
	qdevice(WINQUIT);	/* Need to turn off stereo on quit */
	qdevice(LEFTMOUSE);   /* Eat up mouse events */
	qdevice(MIDDLEMOUSE);   /* Eat up mouse events */

	background();
	while (dev = qread(&val)) 
	{
		switch(dev)
		{
		case REDRAW:
			background();
			break;
		case ESCKEY:
		case WINQUIT:
			stereo_off();	/* From libstereo.a */
			exit(0);
		}
	}
}

void
background()
{
	color(9);
	clear();

	color(10);
	draw_pattern();
}

void
draw_pattern()
{
	int i, j;

	color(BLACK);
	rectfi(0, YMAXSTEREO, XMAXSCREEN, YOFFSET);

	for (i=0; i<=XMAXSCREEN; i+=40)
	{
		move2i(i-SEP, 0);
		draw2i(i-SEP, YMAXSTEREO);

		move2i(i+SEP, 0+YOFFSET);
		draw2i(i+SEP, YMAXSTEREO+YOFFSET);
	}

	for (j=0; j<=YMAXSTEREO; j+=20)
	{
		move2i(0, j);
		draw2i(XMAXSCREEN, j);

		move2i(0, j+YOFFSET);
		draw2i(XMAXSCREEN, j+YOFFSET);
	}
}
