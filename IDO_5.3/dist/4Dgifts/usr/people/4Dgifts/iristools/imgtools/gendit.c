/*
 *	gendit - 
 *		General image dithering.
 *
 *				Paul Haeberli - 1990
 *
 */
#include "image.h"
#include "dither.h"

short rbuf[8196];
short gbuf[8196];
short bbuf[8196];

#define DITXSIZE	4
#define DITYSIZE	4

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage;
    dither *rdit, *gdit, *bdit;
    int rlevels, glevels, blevels;
    int y, xsize, ysize, zsize;
    int spaceout;

    if( argc<6 ) {
	fprintf(stderr,"usage: gendit inimage.rgb outimage.rgb nr ng nb [-s]\n");
	exit(1);
    } 
    if(argc>6)
	spaceout = 1;
    else
	spaceout = 0;
    iimage=iopen(argv[1],"r");
    if(!iimage) {
	fprintf(stderr,"ditimg: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    rlevels = atoi(argv[3]);
    glevels = atoi(argv[4]);
    blevels = atoi(argv[5]);
    rdit = newdither(dithbayer44,DITXSIZE,DITYSIZE,rlevels);
    gdit = newdither(dithbayer44,DITXSIZE,DITYSIZE,glevels);
    bdit = newdither(dithbayer44,DITXSIZE,DITYSIZE,blevels);
    if(spaceout) {
        rolldither(rdit,DITXSIZE/2,0);
        rolldither(bdit,0,DITYSIZE/2);
    }
    if(zsize>=3) {
	oimage = iopen(argv[2],"w",RLE(1),3,xsize,ysize,3);
	for(y=0; y<ysize; y++) {
	    getrow(iimage,rbuf,y,0);
	    getrow(iimage,gbuf,y,1);
	    getrow(iimage,bbuf,y,2);

	    ditrow(rdit,rbuf,y,xsize);
	    ditrow(gdit,gbuf,y,xsize);
	    ditrow(bdit,bbuf,y,xsize);

	    ditherexp(rdit,rbuf,xsize);
	    ditherexp(gdit,gbuf,xsize);
	    ditherexp(bdit,bbuf,xsize);

	    putrow(oimage,rbuf,y,0);
	    putrow(oimage,gbuf,y,1);
	    putrow(oimage,bbuf,y,2);
	}
    } else {
	oimage = iopen(argv[2],"w",RLE(1),2,xsize,ysize,1);
	for(y=0; y<ysize; y++) {
	    getrow(iimage,rbuf,y,0);
	    ditrow(rdit,rbuf,y,xsize);
	    ditherexp(rdit,rbuf,xsize);
	    putrow(oimage,rbuf,y,0);
	}
    }
    iclose(oimage);
}
