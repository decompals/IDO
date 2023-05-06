/*
 *	hipass3 - 
 *		Apply a 3 by 3 unsharp mask to hipass filter an image.
 *
 * 		    	Paul Haeberli - 1988
 */
#include "stdio.h"
#include "math.h"
#include "hipass.h"
#include "image.h"

int globalz;
short buf[8192];
IMAGE *iimage, *oimage;

getimgrow(buf,y)
short *buf;
int y;
{
    getrow(iimage,buf,y,globalz);
}

main(argc, argv)
int argc;
char **argv;
{
    int xsize, ysize, zsize;
    int y;
    float mag;
    highpass *hp;

    if (argc<4) {
	fprintf(stderr,"usage: hipass3 inimage outimage mag\n");
	exit(1);
    }
    iimage = iopen(argv[1], "r");
    if (!iimage) {
	fprintf(stderr,"izoom: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    mag = atof(argv[3]);
    hp = newhp(getimgrow,xsize,ysize,mag);
    oimage = iopen(argv[2],"w",RLE(1),3,xsize,ysize,zsize);
    for(globalz=0; globalz<zsize; globalz++) {
	for(y=0; y<ysize; y++) {
	    hpgetrow(hp,buf,y); 
	    putrow(oimage,buf,y,globalz);
	}
    }
    iclose(oimage);
    exit(0);
}
