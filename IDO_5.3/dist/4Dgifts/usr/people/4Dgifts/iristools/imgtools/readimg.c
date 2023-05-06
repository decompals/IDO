/*
 *	readimg - 
 *		Read an RGB image file but don't do anything with it!
 *
 *				Paul Haeberli - 1984
 *
 */
#include "image.h"

short rbuf[8192]; 
short gbuf[8192]; 
short bbuf[8192]; 

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *image;
    register int y, xsize, ysize;
    register int z, zsize;

    if( argc<2 ) {
	fprintf(stderr,"usage: readimg inimage.rgb\n");
	exit(1);
    } 
    if( (image=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"readimg: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = image->xsize;
    ysize = image->ysize;
    zsize = image->zsize;
    if(zsize<3) {
	fprintf(stderr,"readimg: this is not an RGB image file\n");
	exit(1);
    }
    for(y=0; y<ysize; y++) {
	getrow(image,rbuf,y,0);
	getrow(image,gbuf,y,1);
	getrow(image,bbuf,y,2);
    }
    exit(0);
}
