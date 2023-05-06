/*
 *	addborder - 
 *		Surround one image with another to add a border.
 *
 *				Paul Haeberli - 1988
 */
#include "image.h"

short	ibuf[8192];
short	bbuf[8192];
short	obuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    unsigned int xsize, ysize, zsize;
    unsigned int bxsize, bysize, bzsize;
    unsigned int oxsize, oysize;
    IMAGE *iimage, *bimage, *oimage;
    unsigned int y, oy, z;
    int i, width;
    int rgb[4];

    if( argc<4 ) {
	fprintf(stderr,"usage: addborder inimage borderimage outimage\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"addborder: can't open input file %s\n",argv[1]);
	exit(1);
    }
    if( (bimage=iopen(argv[2],"r")) == NULL ) {
	fprintf(stderr,"addborder: can't open input file %s\n",argv[2]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    bxsize = bimage->xsize/2;
    bysize = bimage->ysize/2;
    bzsize = bimage->zsize;
    oxsize = xsize+2*bxsize;
    oysize = ysize+2*bysize;
    oimage = iopen(argv[3],"w",RLE(1),3,oxsize,oysize,zsize); 
    for(z=0; z<zsize; z++) {
	oy = 0;
	for(i=0; i<bysize; i++) {
	    getrow(bimage,bbuf,i,z%bzsize);
	    expandx(bbuf,obuf,bimage->xsize,oxsize);
	    putrow(oimage,obuf,oy++,z);
	}
	getrow(bimage,ibuf,bimage->ysize/2,z%bzsize);
	expandx(ibuf,bbuf,bimage->xsize,oxsize);
	for(y=0; y<ysize; y++) {
	    getrow(iimage,bbuf+bxsize,y,z);
	    putrow(oimage,bbuf,oy++,z);
	}
	for(i=0; i<bysize; i++) {
	    getrow(bimage,bbuf,bimage->ysize-bysize+i,z%bzsize);
	    expandx(bbuf,obuf,bimage->xsize,oxsize);
	    putrow(oimage,obuf,oy++,z);
	}
    }
    iclose(oimage);
    exit(0);
}

expandx(ibuf,obuf,bsize,osize)
register short *ibuf, *obuf;
register int bsize, osize;
{
    register int i, n, val;

    for(i=0; i<bsize/2; i++) 
	*obuf++ = *ibuf++;
    n = osize-2*(bsize/2);
    val = *ibuf;
    for(i=0; i<n; i++) 
	*obuf++ = val;
    for(i=0; i<bsize/2; i++) 
	*obuf++ = *ibuf++;
}
