/*
 *	slide - 
 *		Display a color or black and white image on the iris.  This
 *	program zooms the source image to fill the screen.
 *
 *	  		   	Paul Haeberli - 1989
 */
#include "stdio.h"
#include "gl.h"
#include "device.h"
#include "image.h"
#include "izoom.h"

#define ISCALE    	1.1
#define FILTERTYPE    	BOX

#define XMAX	(xscreensize)
#define YMAX	(yscreensize)

int xscreensize;
int yscreensize;
unsigned short rs[8192];
unsigned short gs[8192];
unsigned short bs[8192];
unsigned char rb[8192];
unsigned char gb[8192];
unsigned char bb[8192];

imagecolor(image)
IMAGE *image;
{
    getrow(image,rs,0,0);
    getrow(image,gs,0,1);
    getrow(image,bs,0,2);
    RGBcolor(rs[0],gs[0],bs[0]);
}

zoom *rz, *gz, *bz;
IMAGE *image;
int x, y, xsize, ysize, zsize;
int backno;
short val;

main(argc,argv)
int argc;
char **argv;
{
    if( argc<2 ) {
	printf("usage: slide infile [backno]\n");
	exit(1);
    } 
    if(argc>2)
	backno = atoi(argv[2]);
    else
	backno = 0;
    xscreensize = getgdesc(GD_XPMAX);
    yscreensize = getgdesc(GD_YPMAX);
    if( (image=iopen(argv[1],"r")) == NULL ) {
	printf("slide: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = image->xsize;
    ysize = image->ysize;
    zsize = image->zsize;
    prefposition(0,xscreensize-1,0,yscreensize-1);
    foreground();
    winopen("slide");
    RGBmode();
    gconfig();
    drawit();
    qdevice(ESCKEY);
    qdevice(LEFTMOUSE);
    while(1) {
	switch(qread(&val)) {
	    case REDRAW:
		drawit();
		break;
	    case ESCKEY:
	    case LEFTMOUSE:
		exit(0);
		break;
	}
    }
}

getr(buf,y)
short *buf;
int y;
{
    getrow(image,buf,y,0);
}

getg(buf,y)
short *buf;
int y;
{
    getrow(image,buf,y,1);
}

getb(buf,y)
short *buf;
int y;
{
    getrow(image,buf,y,2);
}

drawit()
{
    float aspect, waspect;
    int bnx, bny;
    int xorg, yorg;

    aspect = (float)xsize/ysize;
    waspect = (float)XMAX/YMAX;
    if(aspect>waspect) {
	bnx = XMAX;
	bny = (ysize*XMAX)/xsize;
    } else {
	bny = YMAX;
	bnx = (xsize*YMAX)/ysize;
    }
    if(zsize<3) {
	rz = newzoom(getr,xsize,ysize,bnx,bny,FILTERTYPE,1.0);
    } else {
	rz = newzoom(getr,xsize,ysize,bnx,bny,FILTERTYPE,1.0);
	gz = newzoom(getg,xsize,ysize,bnx,bny,FILTERTYPE,1.0);
	bz = newzoom(getb,xsize,ysize,bnx,bny,FILTERTYPE,1.0);
    }
    xorg = (xscreensize-bnx)/2;
    yorg = (yscreensize-bny)/2;
    switch(backno) {
	case 0:
	    RGBcolor(0,0,0);
	    break;
	case 1:
	    RGBcolor(255,255,255);
	    break;
	case 2:
    	    imagecolor(image);
	    break;
    }
    clear();
    reshapeviewport();
    viewport(0,xscreensize-1,0,yscreensize-1);
    ortho2(-0.5,xscreensize-0.5,-0.5,yscreensize-0.5);
    for(y=0; y<bny; y++) {
	if(zsize<3) {
	    getzoomrow(rz,rs,y);
	    stoc(rs,rb,bnx);
	    cmov2i(xorg,yorg+y);
	    writeRGB(bnx,rb,rb,rb); 
	} else {
	    getzoomrow(rz,rs,y);
	    stoc(rs,rb,bnx);
	    getzoomrow(gz,gs,y);
	    stoc(gs,gb,bnx);
	    getzoomrow(bz,bs,y);
	    stoc(bs,bb,bnx);
	    cmov2i(xorg,yorg+y);
	    writeRGB(bnx,rb,gb,bb); 
	}
    }
}
