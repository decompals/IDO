/*
 *	cscale - 
 *		Multiply or divide the colors in an image by a color
 *	vector.
 *
 *				Paul Haeberli - 1988
 */
#include "image.h"
#include "math.h"

short rbuf[8192];
short gbuf[8192];
short bbuf[8192];
int divide;
int balance;

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage;
    unsigned int xsize, ysize, zsize;
    unsigned int y, z;
    int r, g, b;
    int max;

    if(argc<6) {
	fprintf(stderr,"usage: cscale inimage.rgb outimage.rgb r g b [-d] [-b]\n");
	exit(1);
    } 
    r = atoi(argv[3]);
    g = atoi(argv[4]);
    b = atoi(argv[5]);
    if(argc>6) {
	switch(argv[6][1]) {
	    case 'd':
		divide = 1;
		break;
	    case 'b':
		max = r;
		if(g>max)
		   max = g;
		if(b<max)
		   max = b;
		r = ((r*255)/max)+0.5;
		g = ((g*255)/max)+0.5;
		b = ((b*255)/max)+0.5;
		divide = 1;
		break;
	}
    }
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"%s: can't open input file %s\n",argv[0],argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    oimage = iopen(argv[2],"w",RLE(1),3,xsize,ysize,3); 
    for(y=0; y<ysize; y++) {
	if(zsize>=3) {
	    getrow(iimage,rbuf,y,0);
	    getrow(iimage,gbuf,y,1);
	    getrow(iimage,bbuf,y,2);
	} else {
	    getrow(iimage,rbuf,y,0);
	    getrow(iimage,gbuf,y,0);
	    getrow(iimage,bbuf,y,0);
	}
	cscale(rbuf,gbuf,bbuf,xsize,r,g,b,divide);
	putrow(oimage,rbuf,y,0);
	putrow(oimage,gbuf,y,1);
	putrow(oimage,bbuf,y,2);
    }
    iclose(oimage);
    exit(0);
}

cscale(rbuf,gbuf,bbuf,n,r,g,b,div)
register unsigned short *rbuf, *gbuf, *bbuf;
register int n, r, g, b, div;
{
    if(div) {
        while(n--) {
	    *rbuf = (*rbuf * 255)/r;
	    *gbuf = (*gbuf * 255)/g;
	    *bbuf = (*bbuf * 255)/b;
	    if(*rbuf>255)
		*rbuf = 255;
	    if(*gbuf>255)
		*gbuf = 255;
	    if(*bbuf>255)
		*bbuf = 255;
	    rbuf++;
	    gbuf++;
	    bbuf++;
        }
    } else {
        while(n--) {
	    *rbuf = (*rbuf * r)/255;
	    *gbuf = (*gbuf * g)/255;
	    *bbuf = (*bbuf * b)/255;
	    rbuf++;
	    gbuf++;
	    bbuf++;
        }
    }
}
