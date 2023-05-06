/*
 *	rectimg - 
 *		Display a color or black and white image on the iris.  Simple
 *	version for demo use.  This will only work on machines that support
 *	RGB mode.
 *	  		   	Paul Haeberli - 1988
 *
 */
#include "gl.h"
#include "device.h"
#include "image.h"

unsigned long *longimagedata();

IMAGE *image;
int xsize, ysize, zsize;
unsigned long *lptr;
int xscreensize;
int yscreensize;

main(argc,argv)
int argc;
char **argv;
{
    short val;

    if( argc<2 ) {
	printf("usage: rectimg inimage\n");
	exit(1);
    } 
    xscreensize = getgdesc(GD_XPMAX);
    yscreensize = getgdesc(GD_YPMAX);
    sizeofimage(argv[1],&xsize,&ysize);
    printf("size is %d %d\n",xsize,ysize);
    if( xsize > xscreensize || ysize > yscreensize) {
	printf("rectimg: input image is too big %d %d",xsize,ysize);
	exit(1);
    }
    prefsize(xsize,ysize);
    winopen("rectimg");
    RGBmode();
    gconfig();
    lptr = (unsigned long *)longimagedata(argv[1]);
    drawit();
    while(1) {
	switch(qread(&val)) {
	    case REDRAW:
		drawit();
		break;
	}
    }
}

drawit()
{
    reshapeviewport();
    lrectwrite(0,0,xsize-1,ysize-1,lptr);
}
