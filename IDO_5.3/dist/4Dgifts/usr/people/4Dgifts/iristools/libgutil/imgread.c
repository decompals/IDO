/*
 *   	imgread -
 *		Read an image into an array of longs.
 *
 *				Paul Haeberli - 1988
 */
#include "stdio.h"
#include "image.h"

sizeofimage(name, xsize, ysize)
char *name;
int *xsize, *ysize;
{
    IMAGE *image;

    image = iopen(name,"r");
    if(!image) {
	fprintf(stderr,"sizeofimage: can't open image file %s\n",name);
	exit(1);
    }
    *xsize = image->xsize;
    *ysize = image->ysize;
    iclose(image);
}

long *longimagedata(name)
char *name;
{
    long *base, *lptr;
    short *rbuf, *gbuf, *bbuf, *abuf;
    IMAGE *image;
    int y;

    image = iopen(name,"r");
    if(!image) {
	fprintf(stderr,"longimagedata: can't open image file %s\n",name);
	exit(1);
    }
    base = (long *)malloc(image->xsize*image->ysize*sizeof(long));
    rbuf = (short *)malloc(image->xsize*sizeof(short));
    gbuf = (short *)malloc(image->xsize*sizeof(short));
    bbuf = (short *)malloc(image->xsize*sizeof(short));
    abuf = (short *)malloc(image->xsize*sizeof(short));
    if(!base || !rbuf || !gbuf || !bbuf) {
	fprintf(stderr,"longimagedata: can't malloc enough memory\n");
	exit(1);
    }
    lptr = base;
    for(y=0; y<image->ysize; y++) {
	if(image->zsize>=4) {
	    getrow(image,rbuf,y,0);
	    getrow(image,gbuf,y,1);
	    getrow(image,bbuf,y,2);
	    getrow(image,abuf,y,3);
	    rgbatocpack(rbuf,gbuf,bbuf,abuf,lptr,image->xsize);
	    lptr += image->xsize;
	} else if(image->zsize==3) {
	    getrow(image,rbuf,y,0);
	    getrow(image,gbuf,y,1);
	    getrow(image,bbuf,y,2);
	    rgbtocpack(rbuf,gbuf,bbuf,lptr,image->xsize);
	    lptr += image->xsize;
	} else {
	    getrow(image,rbuf,y,0);
	    bwtocpack(rbuf,lptr,image->xsize);
	    lptr += image->xsize;
	}
    }
    iclose(image);
    free(rbuf);
    free(gbuf);
    free(bbuf);
    free(abuf);
    return base;
}

short *shortimagedata(name)
char *name;
{
    short *base, *sptr;
    IMAGE *image;
    int y;

    if( (image=iopen(name,"r")) == NULL ) {
	fprintf(stderr,"shortimagedata: can't open input file %s\n",name);
	exit(1);
    }
    base = (short *)malloc(sizeof(short)*image->xsize*image->ysize);
    sptr = base;
    for(y=0; y<image->ysize; y++) {
	getbwrow(image,sptr,y);
	sptr += image->xsize;
    }
    iclose(image);
    return base;
}

char *charimagedata(name)
char *name;
{
    char *base, *cptr;
    short *rbuf, *gbuf, *bbuf;
    IMAGE *image;
    int y;

    image = iopen(name,"r");
    if(!image) {
	fprintf(stderr,"charimagedata: can't open image file %s\n",name);
	exit(1);
    }
    base = (char *)malloc(image->xsize*image->ysize*sizeof(char));
    rbuf = (short *)malloc(image->xsize*sizeof(short));
    if(!base || !rbuf) {
	fprintf(stderr,"charimagedata: can't malloc enough memory\n");
	exit(1);
    }
    cptr = base;
    for(y=0; y<image->ysize; y++) {
	getrow(image,rbuf,y,0);
	stoc(rbuf,cptr,image->xsize);
	cptr += image->xsize;
    }
    iclose(image);
    free(rbuf);
    return base;
}
