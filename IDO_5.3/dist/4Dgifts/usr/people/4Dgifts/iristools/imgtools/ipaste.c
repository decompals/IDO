/*
 *	ipaste - 
 *		Display an image on the iris screen.
 *
 *			Paul Haeberli - 1984	
 *
 */
#include <gl/gl.h>
#include <gl/device.h>
#include "image.h"
#include "port.h"
#include "dispimg.h"

#define XBORDER		8
#define TOPBORDER	8
#define BOTBORDER	40
#define MINWINXSIZE     75

typedef struct PASTEIMAGE {
    int xsize, ysize, zsize;
    int xorg, yorg;
    int sxmode, dpp;
    int xframe, yframe;
    int xdraw, ydraw;
    DISPIMAGE *di;
} PASTEIMAGE;

PASTEIMAGE *pi;
PASTEIMAGE *readit();
int drawit();
int drawfunc();

int xmaxscreen;
int ymaxscreen;

main(argc,argv)
int argc;
char **argv;
{
    register int i;
    short val;
    int wx, wy, preforg, sxmode;
    char *inimg;

    if( argc<2 ) {
	fprintf(stderr,"usage: ipaste [-sx] [-f] [-n] [-o xorg yorg] inimage\n");
	exit(1);
    } 
    xmaxscreen = getgdesc(GD_XPMAX)-1;
    ymaxscreen = getgdesc(GD_YPMAX)-1;
    sxmode = 0;
    preforg = 0;
    for(i=1; i<argc; i++) {
	if (argv[i][0] == '-') {
	    if(strcmp(argv[i],"-sx") == 0)
		sxmode++;
	    if(strcmp(argv[i],"-f") == 0)
		foreground();
	    if(strcmp(argv[i],"-n") == 0)
		noborder();
	    if(strcmp(argv[i],"-o") == 0) {
		i++;
		wx = atoi(argv[i]);
		i++;
		wy = atoi(argv[i]);
		preforg = 1;
	    }
	} else {      /* azzume we've got a legit image file at this point */
	    inimg = argv[i];
	}
    }
    pi = readit(inimg,sxmode,wx,wy,preforg);
    qdevice(ESCKEY);
    drawfunc(drawit);
}

PASTEIMAGE *readit(filename,sxmode,wx,wy,preforg)
char *filename;
int sxmode, wx, wy, preforg;
{
    register IMAGE *image;
    register PASTEIMAGE *pi;
    register int y;
    register short *srow;
    unsigned char *rrow, *grow, *brow;

/* allocate the image struct, and open the input file */
    pi = (PASTEIMAGE *)malloc(sizeof(PASTEIMAGE));
    if( (image=iopen(filename,"r")) == NULL ) {
	fprintf(stderr,"paste: can't open input file %s\n",filename);
	exit(1);
    }

/* calculate the window size */
    pi->xsize = image->xsize;
    pi->ysize = image->ysize;
    if(sxmode) {
	pi->sxmode = 1;
	pi->xframe = pi->xsize+XBORDER+XBORDER;
	pi->yframe = pi->ysize+BOTBORDER+TOPBORDER;
	if(pi->xframe>xmaxscreen+1)
	    pi->xframe = xmaxscreen+1;
	if(pi->yframe>ymaxscreen+1)
	    pi->yframe = ymaxscreen+1;
 	pi->xdraw = pi->xframe-XBORDER-XBORDER;
 	pi->ydraw = pi->yframe-BOTBORDER-TOPBORDER;
	pi->xorg = XBORDER;
	pi->yorg = BOTBORDER;
	noborder();
    } else if (pi->xsize < MINWINXSIZE) {
	pi->sxmode = 0;
	pi->xframe = MINWINXSIZE;
	pi->yframe = pi->ysize;
	if(pi->yframe>ymaxscreen+1)
	    pi->yframe = ymaxscreen+1;
 	pi->xdraw = pi->xsize;
 	pi->ydraw = pi->ysize;
	pi->xorg = (MINWINXSIZE - pi->xdraw) / 2;
	pi->yorg = 0;
    } else {
	pi->sxmode = 0;
	pi->xframe = pi->xsize;
	pi->yframe = pi->ysize;
	if(pi->xframe>xmaxscreen+1)
	    pi->xframe = xmaxscreen+1;
	if(pi->yframe>ymaxscreen+1)
	    pi->yframe = ymaxscreen+1;
 	pi->xdraw = pi->xframe;
 	pi->ydraw = pi->yframe;
	pi->xorg = 0;
	pi->yorg = 0;
    }

/* open the window */
    if(preforg) {
	prefposition(wx,wx+pi->xframe-1,wy,wy+pi->yframe-1);
	prefsize(pi->xframe,pi->yframe);
	winopen("ipaste");
	wintitle(filename);
    } else {
	prefsize(pi->xframe,pi->yframe);
	winopen("ipaste");
	wintitle(filename);
    }

/* set the display mode for the image */

    setimagemode(image);
    grey(0.5);
    clear();
    makeframe(pi);
    pi->di = makedisprgn(image,0,pi->xdraw-1,0,pi->ydraw-1,1,pi->xorg,pi->yorg);
    iclose(image);
    return pi;
}

drawit()
{
    if(pi->xorg>0) {
        grey(0.5);
        clear();
    }
    makeframe(pi);
    drawimage(pi->di,pi->xorg,pi->yorg);
}

makeframe(pi)
PASTEIMAGE *pi;
{
    reshapeviewport();
    viewport(0,pi->xframe-1,0,pi->yframe-1);
    ortho2(-0.5,pi->xframe-0.5,-0.5,pi->yframe-0.5);
    if(pi->sxmode) {
	grey(1.0);
	rectfi(0,0,pi->xframe-1,BOTBORDER-1);
	rectfi(0,0,XBORDER-1,pi->yframe-1);
	rectfi(pi->xframe-XBORDER,0,pi->xframe-1,pi->yframe-1);
	rectfi(0,pi->yframe-TOPBORDER,pi->xframe-1,pi->yframe-1);
	grey(0.0);
	recti(XBORDER-1,BOTBORDER-1,pi->xframe-XBORDER,pi->yframe-TOPBORDER);
	grey(0.0);
        recti(0,0,pi->xframe-1,pi->yframe-1);
    }
}
