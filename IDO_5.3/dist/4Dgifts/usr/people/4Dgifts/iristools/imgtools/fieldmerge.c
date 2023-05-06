/*
 *	fieldmerge - 
 *		Merge two images to record on fields.
 *
 *				Paul Haeberli - 1988
 */
#include "image.h"

short buf[8192];

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *iimage0, *iimage1, *oimage;
    register int i, y, z;
    int xsize, ysize, zsize;

    if( argc<4 ) {
	fprintf(stderr,"usage: fieldmerge inimage0 inimage1 outimage\n");
	exit(1);
    } 
    iimage0 = iopen(argv[1],"r");
    iimage1 = iopen(argv[2],"r");
    xsize = iimage0->xsize;
    ysize = iimage0->ysize;
    zsize = iimage0->zsize;
    if( (iimage1->xsize != xsize) || (iimage1->ysize != ysize) ) {
	fprintf(stderr,"merge: image dimensions must be equal\n");
	exit(1);
    }
    oimage = iopen(argv[3],"w",RLE(1),iimage0->dim,xsize,ysize,zsize); 
    for(z=0; z<zsize; z++) {
	for(y=0; y<ysize; y++) {
	    if(y&1) {
		getrow(iimage1,buf,y,z);
		putrow(oimage,buf,y,z);
	    } else {
		getrow(iimage0,buf,y,z);
		putrow(oimage,buf,y,z);
	    }
	}
    }
    iclose(oimage);
    exit(0);
}
