/*
 *	abs - 
 *		Absolute value of an image.
 *
 *				Paul Haeberli - 1986
 */
#include "stdio.h"
#include "port.h"
#include "image.h"

short rowbuf1[8192];
short rowbuf2[8192];

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *iimage1, *oimage;
    register int y, z;
    int xsize, ysize, zsize;

    if( argc<3 ) {
	fprintf(stderr,"usage: abs inimage outimage\n");
	exit(1);
    } 
    if( (iimage1=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"sub: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage1->xsize;
    ysize = iimage1->ysize;
    zsize = iimage1->zsize;
    oimage = iopen(argv[2],"w",RLE(2),iimage1->dim,xsize,ysize,zsize); 
    isetname(oimage,iimage1->name);
    for(z=0; z<zsize; z++)
	for(y=0; y<ysize; y++) {
	    getrow(iimage1,rowbuf1,y,z);
	    absrow(rowbuf1,rowbuf2,xsize);
	    putrow(oimage,rowbuf2,y,z);
	}
    iclose(oimage);
    exit(0);
}

absrow(buf1,buf2,n)
register short *buf1;
register short *buf2;
register int n;
{
    while (n--) {
	if(*buf1<128)
	    *buf2 = (255*(128-(*buf1)))/128;
	else
	    *buf2 = (255*(*buf1-128))/128;
	buf1++;
	buf2++;
    }
}
