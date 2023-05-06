/*
 *	frombin -
 *		Convert a binary RGB dump image to an Iris image.
 *
 *				Paul Haeberli - 1986
 */
#include "image.h"

#define MAXSIZE	8192

char cbuf[4*MAXSIZE];
short sbuf[MAXSIZE];
short rbuf[MAXSIZE];
short gbuf[MAXSIZE];
short bbuf[MAXSIZE];
int xsize, ysize, zsize;

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *image;
    int y, z;
    char dname[128];
    FILE *inf;
    int band = 0;
    int interleave;

    if(argc<5) {
	fprintf(stderr,"usage: frombin inimage.bin outimage.rgb xsize ysize [zsize] [-i]\n");
	exit(1);
    }
    inf = fopen(argv[1],"r");
    if(!inf) {
	fprintf(stderr,"frombin: can't open %s\n",argv[1]);
	exit(1);
    }
    xsize = atoi(argv[3]);
    ysize = atoi(argv[4]);
	interleave = 1;
    if(argc>5) {
        zsize = atoi(argv[5]);
	    interleave = 0;
	if(argc>6)
	    interleave = 1;
    } else {
	zsize = 1;
    }
    image = iopen(argv[2],"w",RLE(1),3,xsize,ysize,zsize);
    if(interleave) {
	for(y=0; y<ysize; y++) {
	    fread(cbuf,1,xsize*zsize,inf);
	    for(z=0; z<zsize; z++) {
		uninterleave(cbuf,sbuf,xsize,zsize,z);
		putrow(image,sbuf,y,z);
	    }
	}
    } else {
	for(z=0; z<zsize; z++) {
	    for(y=0; y<ysize; y++) {
		if(fread(cbuf,1,xsize,inf) == xsize) 
		    ctos(cbuf,sbuf,xsize);
		putrow(image,sbuf,y,z);
	    }
	}
    }
    iclose(image);
    exit(0);
}

uninterleave(cptr,sptr,n,nslices,slice)
unsigned char *cptr;
short *sptr;
int n, nslices, slice;
{
    cptr += slice;
    while(n--) {
	*sptr++ = *cptr;
	cptr += nslices;
    }
}
