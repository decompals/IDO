/*
 *	hypwarp - 
 *		Lighten or darken an image by hyperbola warping.
 *
 *				Paul Haeberli - 1988
 */
#include "image.h"
#include "math.h"
#include "lut.h"

short buf[8192];

float hypval, powval;
float hypfunc();

float tabfunc(v)
float v;
{
    return hypfunc(hypval,powval,v);
}

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage;
    unsigned int xsize, ysize, zsize;
    unsigned int y, z;
    lut *l;

    if( argc<5 ) {
	fprintf(stderr,"usage: hypwarp inimage outimage hyp pow\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"%s: can't open input file %s\n",argv[0],argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    hypval = atof(argv[3]);
    powval = atof(argv[4]);
    l = makelut(tabfunc,256,256,0);
    oimage = iopen(argv[2],"w",RLE(1),iimage->dim,xsize,ysize,iimage->zsize); 
    for(z=0; z<zsize; z++) {
	for(y=0; y<ysize; y++) {
	    getrow(iimage,buf,y,z);
	    applylut(l,buf,xsize);
	    putrow(oimage,buf,y,z);
	}
    }
    iclose(oimage);
    exit(0);
}
