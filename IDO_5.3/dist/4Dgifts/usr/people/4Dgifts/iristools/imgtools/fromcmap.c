/*
 *	fromcmap - 
 *		Convert a color map into an image with one row.
 *
 *				Paul Haeberli - 1987
 */
#include "port.h"
#include "image.h"
#include "stdio.h"

short r[8192];
short g[8192];
short b[8192];
short rowbuf[20]; 

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *iimage, *oimage;
    register int y, z;
    int ysize;
    int n;

    if( argc<3 ) {
	fprintf(stderr,"usage: fromcmap color.map outimage.rgb\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"fromcamp: can't open input file %s\n",argv[1]);
	exit(1);
    }
    ysize = iimage->ysize;
    if( (oimage=iopen(argv[2],"w",RLE(1),3,ysize,1,3)) == NULL ) {
	fprintf(stderr,"fromcamp: can't open output file %s\n",argv[2]);
	exit(1);
    }
    for(y=0; y<ysize; y++) {
	getrow(iimage,rowbuf,y,0);
	r[y] = rowbuf[1];
	g[y] = rowbuf[2];
	b[y] = rowbuf[3];
    }
    putrow(oimage,r,0,0);
    putrow(oimage,g,0,1);
    putrow(oimage,b,0,2);
    iclose(oimage);
    exit(0);
}
