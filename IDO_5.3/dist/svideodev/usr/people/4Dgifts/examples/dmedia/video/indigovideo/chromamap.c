/*
 * chromamap.c
 *
 *  This program demonstrates how to manipulate the IndigoVideo
 *  chroma key map for programs that use the video overlay feature.
 *  It shows the colors that correspond to the 256 entries in the map,
 *  where 0 is the lower left corner and 255 is the upper right.
 *  Clicking the left mouse button over a color toggles the value for that
 *  entry. An X mark in a box means that color will be keyed out (i.e.,
 *  the underlying graphics will show through.) A pull-down menu can
 *  be used to clear or set all of the entries. The program initializes
 *  all entries to 0 when it starts up.
 *
 *  To demonstrate keying, compile and run the voverlay program in
 *  this directory. As you set entries in the key map, the graphics
 *  in voverlay will begin to appear. If you set all of the entries,
 *  only the graphics will appear. If you clear all of the entries,
 *  only video will appear.
 */

#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <svideo.h>

static SVhandle V;
static SVcolorMap keymap;

static long rgb8to32[256];

#define grey9()	cpack(0x00E0E0E0)
#define grey7()	cpack(0x00B0B0B0)
#define grey5()	cpack(0x00808080)
#define bordercolor()	grey5()

static long xsize, ysize;
static long xorg, yorg;
#define YSIZE		16
#define XSIZE		16
#define BORDERSIZE	0.25
#define MOUSEXMAP(x)	( ((XSIZE+2*BORDERSIZE)*((x)-xorg))/(xsize) )
#define MOUSEYMAP(y)	( ((YSIZE+2*BORDERSIZE)*((y)-yorg))/(ysize) )

static void
drawX(int i, int j)
{
    grey7();
    move2i(i,j);
    draw2i(i+1,j+1);
    move2i(i,j+1);
    draw2i(i+1,j);
}

static void
drawborder(int i, int j)
{
    bordercolor();
    move2i(i,j);
    draw2i(i+1,j);
    draw2i(i+1,j+1);
    draw2i(i,j+1);
    draw2i(i,j);
}

static void
drawcolor(int i, int j)
{
    cpack(rgb8to32[(j*XSIZE)+i]);
    rectfi(i,j,i+1,j+1);
}

static void
showmap(void)
{
    int i, j;

    /* Clear background */
    grey9();
    clear();

    ortho2(-BORDERSIZE, XSIZE+BORDERSIZE, -BORDERSIZE,YSIZE+BORDERSIZE);

    /* Draw colored boxes for the 256 RGB colors */
    for (j=0; j<YSIZE; j++) {
	for (i=0; i<XSIZE; i++) {
	    drawcolor(i,j);
	    if (keymap[i+(j*XSIZE)].red) {
		drawX(i,j);
	    }
	}
    }

    /* Draw borders around all the boxes */
    bordercolor();
    if ((xsize/XSIZE)>4) {
	for (j=0; j<=YSIZE; j++) {
	    move2i(0,j);
	    draw2i(XSIZE,j);
	}
	for (j=0; j<=XSIZE; j++) {
	    move2i(j,0);
	    draw2i(j,YSIZE);
	}
    }
}

static void
fillmap(int fill)
{
    int i;
    for (i = 0; i < SV_CMAP_SIZE; i++)
	keymap[i].red = fill;
    showmap();
    if (svLoadMap(V, SV_CHROMA_KEY_MAP, keymap) < 0)
	printf("load map failed\n");
}

main(void)
{
    short val;
    int menu;
    int r, g, b;
    float mx, my;

    /* Open video device */
    if ((V = svOpenVideo()) == NULL) {
	svPerror("open");
	exit(1);
    }

    /* Create mapping of 8-bit RGB to 32-bit equivalents */
    for (r=0; r<8; r++) {
	for (b=0; b<4; b++) {
	    for (g=0; g<8; g++) {
		rgb8to32[(r<<5)|(b<<3)|g] =
		     ((r<<5)|(r<<2)|(r>>1))    |
		     (((g<<5)|(g<<2)|(g>>1)) << 8 )  |
		     (((b<<6)|(b<<4)|(b<<2)|b) << 16);
	    }
	}
    }

    keepaspect(XSIZE, YSIZE);
    winopen("chromamap");
    RGBmode();
    gconfig();

    qdevice(LEFTMOUSE);
    qdevice(MOUSEX);
    qdevice(MOUSEY);
    qdevice(MENUBUTTON);
    menu = defpup("chromamap %t|clear all|set all|exit");

    getsize(&xsize,&ysize);
    getorigin(&xorg,&yorg);

    /* Put map in known state */
    fillmap(0);

    while (1) {
	switch(qread(&val)) {
	    case REDRAW:
		reshapeviewport();
		getsize(&xsize,&ysize);
		getorigin(&xorg,&yorg);
		showmap();
		break;

	    case MOUSEX:
		mx = MOUSEXMAP(val) - .25;
		if (mx < 0.0)
		    mx = 0.0;
		else if (mx >= XSIZE)
		    mx = XSIZE-1;
		break;

	    case MOUSEY:
		my = MOUSEYMAP(val) - .25;
		if (my < 0.0)
		    my = 0.0;
		else if (my >= YSIZE)
		    my = YSIZE-1;
		break;

	    case LEFTMOUSE:
		/* Toggle the entry's key */
		if (val) {
		    int i = (int)mx + (int)my * XSIZE;

		    keymap[i].red = !keymap[i].red;

		    drawcolor((int)mx, (int)my);
		    if (keymap[i].red) {
			drawX((int)mx, (int)my);
		    }
		    drawborder((int)mx, (int)my);

		    if (svLoadMap(V, SV_CHROMA_KEY_MAP, keymap) < 0)
			printf("load map failed\n");
		}
		break;

	    case MENUBUTTON:
		if (val) {
		    switch (dopup(menu)) {
			case 1:
			    fillmap(0);
			    break;
			case 2:
			    fillmap(1);
			    break;
			case 3:
			    exit(0);
		    }
		}
		break;

	}
    }
}
