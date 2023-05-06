/*
 *	invert - 
 *		Invert an image.
 *
 *				Paul Haeberli - 1986
 */
#include "image.h"
#include "stdio.h"

short rowbuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *iimage, *oimage;
    register int y, z;
    int xsize, ysize, zsize;

    if( argc<3 ) {
	fprintf(stderr,"usage: invert inimage outimage\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"invert: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    oimage = iopen(argv[2],"w",RLE(1),iimage->dim,xsize,ysize,zsize); 
    isetname(oimage,iimage->name);
    for(z=0; z<zsize; z++)
	for(y=0; y<ysize; y++) {
	    getrow(iimage,rowbuf,y,z);
	    invert(rowbuf,xsize);
	    putrow(oimage,rowbuf,y,z);
	}
    iclose(oimage);
    exit(0);
}

invert(buf,n)
register short *buf;
register int n;
{
    while (n--) {
	*buf = 255-*buf;
	buf++;
    }
}
