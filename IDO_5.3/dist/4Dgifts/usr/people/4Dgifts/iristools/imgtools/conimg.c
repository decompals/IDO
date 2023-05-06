/*
 *	conimg - 
 *	    	Make a constant image.
 *
 *				Paul Haeberli - 1989
 */
#include "image.h"

short rowbuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *oimage;
    int xsize, ysize, zsize;
    int x, y, z, i;
    int seed;

    if(argc<5) {
	fprintf(stderr,"usage: conimg outimage xsize ysize [bwval] [r g b]\n");
	exit(1);
    }
    xsize = atoi(argv[2]);
    ysize = atoi(argv[3]);
    zsize = argc-4;
    oimage = iopen(argv[1],"w",RLE(1),3,xsize,ysize,zsize);
    for(z=0; z<zsize; z++) {
	setrow(rowbuf,atoi(argv[4+z]),xsize);
	for(y=0; y<ysize; y++) 
	    putrow(oimage,rowbuf,y,z);
    }
    iclose(oimage);
    exit(0);
}
