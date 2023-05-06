/*
 *	halftone - 
 *		Half-tone an image using Hollaway's technique.
 *
 *			    Paul Haeberli - 1990
 */
#include "stdio.h"
#include "math.h"
#include "image.h"
#include "setscreen.h"

#define PAT_DOTS 	0
#define PAT_LINES	1
#define PAT_SINE	2
#define PAT_WEDGE	3
#define PAT_RECTS	4
#define PAT_SCRATCH	5

short sbuf[8192];
int patternno = PAT_DOTS;

float dotfunc(u,v)
float u, v;
{
    u = u-ffloor(u);
    v = v-ffloor(v);
    u = u-0.5;
    v = v-0.5;
    switch(patternno) {
	case PAT_DOTS:
	    return (u*u+v*v);
	case PAT_LINES:
	    if(u<0.0)
		return -u;
	    else
		return u;
	case PAT_SINE:
    	    return cos(2*M_PI*v)+cos(2*M_PI*u);
	case PAT_WEDGE:
	    u+=0.5;
	    v+=0.5;
	    u = u/2.0;
	    return (u*u+v*v);
	case PAT_RECTS:
	    return u/2.0+v;
	case PAT_SCRATCH:
	    if(u<0.0)
	       u = -u;
	    if(v<0.0)
	       v = -v;
	    v = v/2.0;
	    if(u>v)
	       return u;
	    else
	       return v;
    }
}

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *iimage, *oimage;
    register int y, z;
    int xsize, ysize, zsize;
    float freq, angle;
    halftone *ht;

    if( argc<5 ) {
	fprintf(stderr,"usage: halftone inimage outimage freq angle [patternno]\n");
	exit(1);
    } 
    if(argc>5)
	patternno = atoi(argv[5]);
    iimage=iopen(argv[1],"r");
    if (!iimage) {
	fprintf(stderr,"halftone: can't open input file %s\n",argv[1]);
	exit(1);
    }
    freq = atof(argv[3]);
    angle = atof(argv[4]);
    ht = setscreen(freq,angle,dotfunc);
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    oimage = iopen(argv[2],"w",RLE(1),iimage->dim,xsize,ysize,zsize); 
    for(z=0; z<zsize; z++) {
	for(y=0; y<ysize; y++) {
	    getrow(iimage,sbuf,y,z);
	    screenrow(ht,sbuf,xsize,0,y);
	    putrow(oimage,sbuf,y,z);
	}
    }
    iclose(oimage);
    exit(0);
}
