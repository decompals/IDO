/*
 *	verbatum - 
 *		Force an image to be stored in verbatum format.
 *
 *				Paul Haeberli - 1985
 */
#include "image.h"

short rowbuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *iimage, *oimage;
    register int y, z;
    int xsize, ysize, zsize;

    if( argc<3 ) {
	fprintf(stderr,"usage: verbatim inimage outimage\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"verbatim: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    oimage = iopen(argv[2],"w",VERBATIM(BPP(iimage->type)),
					iimage->dim,xsize,ysize,zsize); 
    isetname(oimage,iimage->name);
    for(z=0; z<zsize; z++)
	for(y=0; y<ysize; y++) {
	    getrow(iimage,rowbuf,y,z);
	    putrow(oimage,rowbuf,y,z);
	}
    iclose(oimage);
    exit(0);
}
