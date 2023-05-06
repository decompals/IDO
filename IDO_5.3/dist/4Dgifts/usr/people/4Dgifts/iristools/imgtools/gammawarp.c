/*
 *	gammawarp - 
 *		Lighten or darken an image by changing the gamma.
 *
 *				Paul Haeberli - 1986
 */
#include "image.h"
#include "math.h"
#include "lut.h"

short buf[8192];
float igamma;

float transfunc();

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage;
    unsigned int xsize, ysize, zsize;
    unsigned int y, z;
    int randmode;
    lut *l;

    if( argc<4 ) {
	fprintf(stderr,"usage: gammawarp inimage outimage gamma [-r]\n");
	exit(1);
    } 
    if(argc>4)
	randmode = 1;
    else
	randmode = 0;
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"%s: can't open input file %s\n",argv[0],argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    igamma = atof(argv[3]);
    if(randmode)
	l = makelut(transfunc,256,256,1);
    else
	l = makelut(transfunc,256,256,0);
    oimage = iopen(argv[2],"w",RLE(1),iimage->dim,xsize,ysize,iimage->zsize); 
    isetname(oimage,iimage->name);
    oimage->colormap = iimage->colormap;
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

float transfunc(f)
float f;
{
    return pow(f,igamma);
}
