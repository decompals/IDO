/*
 *	xzoom- 
 *		Magnify or minify a picture in the x direction.
 *
 * 		    	Paul Haeberli - 1990
 */
#include "image.h"
#include "izoom.h"
#include "math.h"

int globaly, globalz;
IMAGE *aimage, *bimage;
int anx, any, bnx, bny;
short buf[8192];

getimgrow(buf,y)
short *buf;
int y;
{
    getrow(aimage,buf,globaly,globalz);
}

main(argc, argv)
int argc;
char **argv;
{
    char *afile, *bfile;
    float sx;
    int i, y, mode, maxdim;
    float blur;
    zoom *z;

    if (argc<3) {
	fprintf(stderr,"usage: xzoom inimage outimage xscale [-i -b -t -q -m or -g]\n");
	fprintf(stderr,"                                            [-w blurfactor]\n");
	exit(1);
    }
    afile = argv[1];
    bfile = argv[2];
    mode = TRIANGLE;
    blur = 1.0;
    for(i=4; i<argc; i++) {
        if(strcmp(argv[i],"-i") == 0)
	   mode = IMPULSE;
	else if(strcmp(argv[i],"-b") == 0)
	   mode = BOX;
	else if(strcmp(argv[i],"-t") == 0)
	   mode = TRIANGLE;
	else if(strcmp(argv[i],"-q") == 0)
	   mode = QUADRATIC;
	else if(strcmp(argv[i],"-m") == 0)
	   mode = MITCHELL;
	else if(strcmp(argv[i],"-g") == 0) 
	   mode = GAUSSIAN;
	else if(strcmp(argv[i],"-w") == 0) {
	   i++;
	   blur = atof(argv[i]);
	} else {
	    fprintf(stderr,"izoom: bad option %s\n",argv[5]);
	    exit(1);
	}
    }

/* open input image */
    aimage = iopen(afile, "r");
    if (!aimage) {
	fprintf(stderr,"izoom: can't open input file %s\n", afile);
	exit(1);
    }

    sx = atof(argv[3]);

/* calculate tile size */
    anx = aimage->xsize;	
    any = aimage->ysize;
    bnx = (anx*sx)+0.5;
    bny = any;

/* open output file */
    bimage = iopen(bfile,"w",RLE(BPP(aimage->type)),3,bnx,bny,aimage->zsize);
    z = newzoom(getimgrow,anx,1,bnx,1,mode,blur);
    for(globalz=0; globalz<aimage->zsize; globalz++) {
	for(y=0; y<bny; y++) {
	    globaly = y;
	    getzoomrow(z,buf,0);
	    putrow(bimage,buf,y,globalz);
	}
    }
    iclose(aimage);
    iclose(bimage);
    exit(0);
}
