/*
 *	randimg - 
 *	    	Make a noise image.
 *
 *				Paul Haeberli - 1988
 */
#include "image.h"

short rowbuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *oimage;
    int xsize, ysize, x, y, z, i;
    int seed;

    if(argc<4) {
	fprintf(stderr,"usage: randimg outimage xsize ysize [seed]\n");
	exit(1);
    }
    xsize = atoi(argv[2]);
    ysize = atoi(argv[3]);
    oimage = iopen(argv[1],"w",RLE(1),2,xsize,ysize,1);
    random();
    if(argc>4) {
	seed = atoi(argv[4]);
	if(seed>0)
	    srandom(seed);
	else
	    srandom(getpid());
    }
    for(y=0; y<ysize; y++) {
	randrow(rowbuf,xsize);
	putrow(oimage,rowbuf,y,z);
    }
    iclose(oimage);
    exit(0);
}

randrow(buf,n)
short *buf;
int n;
{
    while(n--)
	*buf++ = random()&0xff;
}
