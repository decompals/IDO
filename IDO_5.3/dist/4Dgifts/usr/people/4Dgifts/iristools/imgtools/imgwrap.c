/*
 *	imgwrap - 
 *		Shift image values left one bit.
 *
 *				Paul Haeberli - 1987
 */
#include "image.h"

short	buf[8192];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage;
    unsigned int xsize, ysize, zsize;
    unsigned int y, z;
    int min, max;

    if( argc<3 ) {
	fprintf(stderr,"usage: imgwrap inimage outimage\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"imgwrap: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    oimage = iopen(argv[2],"w",RLE(1),iimage->dim,xsize,ysize,zsize); 
    isetname(oimage,iimage->name);
    oimage->colormap = iimage->colormap;
    for(z=0; z<zsize; z++) {
	for(y=0; y<ysize; y++) {
	    getrow(iimage,buf,y,z);
	    imgwrap(buf,xsize);
	    putrow(oimage,buf,y,z);
	}
    }
    iclose(oimage);
    exit(0);
}

imgwrap(sptr,n)
register unsigned short *sptr;
register int n;
{
    while(n--) {
	*sptr = (*sptr<<1)&0xff;
	sptr++;
    }
}
