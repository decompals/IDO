/*
 *	cglue - 
 *		Glue together 3 images into a color image
 *
 *				Paul Haeberli - 1986
 */
#include "image.h"

short rowbuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *rimage, *gimage, *bimage, *aimage, *oimage;
    unsigned int xsize, ysize;
    unsigned int y;
    int havealpha;

    havealpha = 0;
    if( argc<5 ) {
	printf("usage: cglue red.bw grn.bw blu.bw [alp.bw] outimage.rgb\n");
	exit(0);
    } 
    if(argc>5)
	havealpha++;
    if( (rimage=iopen(argv[1],"r")) == NULL ) {
	printf("cglue: can't open input file %s\n",argv[1]);
	exit(0);
    }
    if( (gimage=iopen(argv[2],"r")) == NULL ) {
	printf("cglue: can't open input file %s\n",argv[2]);
	exit(0);
    }
    if( (bimage=iopen(argv[3],"r")) == NULL ) {
	printf("cglue: can't open input file %s\n",argv[3]);
	exit(0);
    }
    if(havealpha) {
	if( (aimage=iopen(argv[4],"r")) == NULL ) {
	    printf("cglue: can't open input file %s\n",argv[3]);
	    exit(0);
	}
	oimage = iopen(argv[5],"w",RLE(1),3,rimage->xsize,rimage->ysize,4); 
    } else {
	oimage = iopen(argv[4],"w",RLE(1),3,rimage->xsize,rimage->ysize,3); 
    }
    isetname(oimage,rimage->name);
    ysize = rimage->ysize;
    xsize = rimage->xsize;
    for(y=0; y<ysize; y++) {
	getrow(rimage,rowbuf,y,0);
	putrow(oimage,rowbuf,y,0);
    }
    for(y=0; y<ysize; y++) {
	getrow(gimage,rowbuf,y,0);
	putrow(oimage,rowbuf,y,1);
    }
    for(y=0; y<ysize; y++) {
	getrow(bimage,rowbuf,y,0);
	putrow(oimage,rowbuf,y,2);
    }
    if(havealpha) {
	for(y=0; y<ysize; y++) {
	    getrow(aimage,rowbuf,y,0);
	    putrow(oimage,rowbuf,y,3);
	}
    }
    iclose(oimage);
    exit(0);
}
