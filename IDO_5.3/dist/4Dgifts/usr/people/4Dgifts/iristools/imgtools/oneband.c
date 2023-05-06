/*
 *	oneband - 
 *		Extract  a single band of a color image.
 *
 *				Paul Haeberli - 1986
 */
#include "image.h"

short	rbuf[8192];
short	gbuf[8192];
short	bbuf[8192];
short	obuf[8192];
int 	band;

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage;
    unsigned int xsize, ysize;
    unsigned int y;

    if( argc<4 ) {
	fprintf(stderr,"usage: oneband inimage.rgb outimage.bw band\n");
	exit(0);
    } 
    band = atoi(argv[3]);
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"oneband: can't open input file %s\n",argv[1]);
	exit(0);
    }
    if(iimage->dim<3) {
	fprintf(stderr,"oneband: %s is not a color image\n",argv[1]);
	exit(0);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    oimage = iopen(argv[2],"w",RLE(1),2,xsize,ysize); 
    for(y=0; y<ysize; y++) {
	getrow(iimage,rbuf,y,band);
	putrow(oimage,rbuf,y,0);
    }
    iclose(oimage);
    exit(0);
}
