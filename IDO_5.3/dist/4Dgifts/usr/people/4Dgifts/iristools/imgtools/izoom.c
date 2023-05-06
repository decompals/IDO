/*
 *	izoom- 
 *		Magnify or minify a picture with or without filtering.  The 
 *	filtered method is one pass, uses 2-d convolution, and is optimized 
 *	by integer arithmetic and precomputation of filter coeffs.
 *
 * 		    	Paul Haeberli and Paul Heckbert - 1988
 */
#include "stdio.h"
#include "math.h"
#include "assert.h"
#include "image.h"
#include "port.h"
#include "izoom.h"

int globalz;
IMAGE *aimage, *bimage;
int anx, any, bnx, bny;

getimgrow(buf,y)
short *buf;
int y;
{
    getrow(aimage,buf,y,globalz);
}

putimgrow(buf,y)
short *buf;
int y;
{
    putrow(bimage,buf,y,globalz);
}

main(argc, argv)
int argc;
char **argv;
{
    char *afile, *bfile;
    float sx, sy;
    int i, mode, maxdim;
    float blur;

    if (argc<4) {
	fprintf(stderr,"usage: izoom inimage outimage xscale yscale [-i -b -t -q -m or -g]\n");
	fprintf(stderr,"                                            [-w blurfactor]\n");
	exit(1);
    }
    afile = argv[1];
    bfile = argv[2];
    mode = TRIANGLE;
    blur = 1.0;
    for(i=5; i<argc; i++) {
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

    if(argc>=5) {
	sx = atof(argv[3]);
	sy = atof(argv[4]);
    } else {
	if(aimage->xsize>aimage->ysize)
	    maxdim = aimage->xsize;
	else
	    maxdim = aimage->ysize;
	sx = sy = atof(argv[3])/maxdim;
	mode = TRIANGLE;
    }

/* calculate tile size */
    anx = aimage->xsize;	
    any = aimage->ysize;
    bnx = (anx*sx)+0.5;
    bny = (any*sy)+0.5;

/* fprintf(stderr,"asize %d %d bsize %d %d scale %f %f\n",anx,any,bnx,bny,sx,sy); */


/* open output file */
    bimage = iopen(bfile,"w",RLE(BPP(aimage->type)),3,bnx,bny,aimage->zsize);
    for(globalz=0; globalz<aimage->zsize; globalz++) 
        filterzoom(getimgrow,putimgrow,anx,any,bnx,bny,mode,blur); 
    iclose(aimage);
    iclose(bimage);
    exit(0);
}
