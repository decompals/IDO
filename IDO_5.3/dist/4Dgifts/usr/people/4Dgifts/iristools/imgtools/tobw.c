/*
 *	tobw - 
 *		Convert a color image into a b/w image by combining the 
 *		three bands. Check lum.h to see contributions"
 *
 *				Paul Haeberli - 1985
 */
#include "image.h"

short	rbuf[8192];
short	gbuf[8192];
short	bbuf[8192];
short	obuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage;
    unsigned int xsize, ysize;
    unsigned int y;

    if( argc<3 ) {
	fprintf(stderr,"usage: tobw inimage.rgb outimge.bw\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"tobw: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    oimage = iopen(argv[2],"w",RLE(1),2,xsize,ysize); 
    for(y=0; y<ysize; y++) {
	if(iimage->zsize<3) {
	    getrow(iimage,obuf,y,0);
	} else {
	    getrow(iimage,rbuf,y,0);
	    getrow(iimage,gbuf,y,1);
	    getrow(iimage,bbuf,y,2);
	    rgbrowtobw(rbuf,gbuf,bbuf,obuf,xsize);
	}
	putrow(oimage,obuf,y,0);
    }
    iclose(oimage);
    exit(0);
}
