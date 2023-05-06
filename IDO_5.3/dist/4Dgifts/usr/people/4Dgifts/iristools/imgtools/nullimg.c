/*
 *	nullimg - 
 *		Make a blank image.
 *
 *				Paul Haeberli - 1986
 */
#include "image.h"

short rbuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    int y, z;
    int xsize, ysize, zsize;
    IMAGE *image;

    if(argc<4) {
	fprintf(stderr,"usage nullimg outimage xsize ysize [zsize]\n");
	exit(1);
    }
    xsize = atoi(argv[2]);
    ysize = atoi(argv[3]);
    if(argc>4) 
	zsize = atoi(argv[4]);
    else
	zsize = 1;
    image = iopen(argv[1],"w",RLE(1),3,xsize,ysize,zsize);
    for(z=0; z<zsize; z++) {
	for(y=0; y<ysize; y++) 
	    putrow(image,rbuf,y,z);
    }
    iclose(image);
    exit(0);
}
