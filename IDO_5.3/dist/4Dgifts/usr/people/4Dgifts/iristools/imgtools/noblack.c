/*
 *	noblack - 
 *		Remove all black from an image.
 *
 *				Paul Haeberli - 1987
 */
#include "image.h"
#include "vect.h"

short rbuf[8192];
short gbuf[8192];
short bbuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage;
    unsigned int xsize, ysize, zsize;
    unsigned int y, z;

    if( argc<2 ) {
	fprintf(stderr,"usage: noblack inimage.rgb outimage.rgb\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"%s: can't open input file %s\n",argv[0],argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    oimage = iopen(argv[2],"w",RLE(1),iimage->dim,xsize,ysize,iimage->zsize); 
    for(y=0; y<ysize; y++) {
	getrow(iimage,rbuf,y,0);
	getrow(iimage,gbuf,y,1);
	getrow(iimage,bbuf,y,2);
	doit(rbuf,gbuf,bbuf,xsize,y);
	putrow(oimage,rbuf,y,0);
	putrow(oimage,gbuf,y,1);
	putrow(oimage,bbuf,y,2);
    }
    iclose(oimage);
    exit(0);
}

doit(rbuf,gbuf,bbuf,n)
register unsigned short *rbuf, *gbuf, *bbuf;
register int n;
{
    while(n--)
	noblack(rbuf++,gbuf++,bbuf++);
}

noblack(r,g,b)
unsigned short *r, *g, *b;
{
    int i;

    i = *r;
    if(*g>i)
	i = *g;
    if(*b>i)
	i = *b;
    if(i>0) {
	*r = (255* *r)/i;
	*g = (255* *g)/i;
	*b = (255* *b)/i;
    } else {
	*r = 255;
	*g = 255;
	*b = 255;
    }
}
