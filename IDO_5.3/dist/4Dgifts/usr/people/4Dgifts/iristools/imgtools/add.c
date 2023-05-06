/*
 *	add - 
 *		Add two images.
 *
 *				Paul Haeberli - 1986
 */
#include "port.h"
#include "image.h"
#include "stdio.h"

short rowbuf1[8192];
short rowbuf2[8192];

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *iimage1, *iimage2, *oimage;
    register int y, z;
    int xsize, ysize, zsize1, zsize2;
    int ozsize;

    if( argc<4 ) {
	fprintf(stderr,"usage: add inimage1 inimage2 outimage\n");
	exit(1);
    } 
    if( (iimage1=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"add: can't open input file %s\n",argv[1]);
	exit(1);
    }
    if( (iimage2=iopen(argv[2],"r")) == NULL ) {
	fprintf(stderr,"add: can't open input file %s\n",argv[2]);
	exit(1);
    }
    xsize = iimage1->xsize;
    ysize = iimage1->ysize;
    zsize1 = iimage1->zsize;
    zsize2 = iimage2->zsize;
    if( (iimage2->xsize != xsize) || (iimage2->ysize != ysize) ) {
	fprintf(stderr,"add: image dimensions must be equal\n");
	exit(1);
    }
    ozsize = MAX(zsize1,zsize2);
    oimage = iopen(argv[3],"w",RLE(1),3,xsize,ysize,ozsize); 
    isetname(oimage,iimage1->name);
    for(z=0; z<ozsize; z++)
	for(y=0; y<ysize; y++) {
	    getrow(iimage1,rowbuf1,y,z%zsize1);
	    getrow(iimage2,rowbuf2,y,z%zsize2);
	    addrow(rowbuf1,rowbuf2,xsize);
	    putrow(oimage,rowbuf2,y,z);
	}
    iclose(oimage);
    exit(0);
}

addrow(buf1,buf2,n)
register short *buf1;
register short *buf2;
register int n;
{
    register int val;

    while (n--) {
	val = *buf2 + *buf1;
	if(val>255)
	   val = 255;
	*buf2 = val;
	buf1++;
	buf2++;
    }
}
