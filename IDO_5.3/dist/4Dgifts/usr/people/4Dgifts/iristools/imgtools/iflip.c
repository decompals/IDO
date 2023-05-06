/*
 *	flip - 
 *		Flip an image about the x, y, xy or yx axis, or rotate
 *	an image 90, 180, or 270 degrees.
 *
 *				Paul Haeberli - 1988
 */
#include "image.h"

short row[8192];

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *iimage, *oimage;
    int oxsize, oysize;
    int bytes, bpp, z;
    int yflip, xflip, trans, flipcode;
    int xsize, ysize, zsize;
    register unsigned char *cbuf, *cptr;
    register short *sbuf, *sptr, *rptr;
    register int x, y;

    if( argc<4 ) {
	fprintf(stderr,"usage: iflip inimage outimage [x y xy yz 90 180 or 270]\n");
	exit(1);
    } 
    iimage=iopen(argv[1],"r");
    if(!iimage) {
	fprintf(stderr,"iflip: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;

    if(strcmp(argv[3],"x") == 0) {
	xflip = 1;
	yflip = 0;
	trans = 0;
    } else if(strcmp(argv[3],"y") == 0) {
	xflip = 0;
	yflip = 1;
	trans = 0;
    } else if(strcmp(argv[3],"xy") == 0) {
	xflip = 0;
	yflip = 0;
	trans = 1;
    } else if(strcmp(argv[3],"yx") == 0) {
	xflip = 1;
	yflip = 1;
	trans = 1;
    } else if(strcmp(argv[3],"0") == 0) {
	xflip = 0;
	yflip = 0;
	trans = 0;
    } else if(strcmp(argv[3],"90") == 0) {
	xflip = 1;
	yflip = 0;
	trans = 1;
    } else if(strcmp(argv[3],"180") == 0) {
	xflip = 1;
	yflip = 1;
	trans = 0;
    } else if(strcmp(argv[3],"270") == 0) {
	xflip = 0;
	yflip = 1;
	trans = 1;
    } else if(strcmp(argv[3],"360") == 0) {
	xflip = 0;
	yflip = 0;
	trans = 0;
    } else {
	fprintf(stderr,"iflip: wierd option %s\n",argv[3]);
	exit(1);
    }
    flipcode = (yflip<<1)+xflip;
    if(trans) {
	oxsize = ysize;
	oysize = xsize;
    } else {
	oxsize = xsize;
	oysize = ysize;
    }
    bpp = BPP(iimage->type);
    oimage = iopen(argv[2],"w",RLE(bpp),iimage->dim,
						oxsize,oysize,iimage->zsize); 
    isetcolormap(oimage,iimage->colormap);

    if(trans) {
	if(bpp == 1) {
	    bytes = xsize*ysize*sizeof(unsigned char);
	    cbuf = (unsigned char *)malloc(bytes);
	    if(!cbuf) {
		fprintf(stderr,"iflip: can't malloc memory\n");
		exit(1);
	    }
	} else {
	    bytes = xsize*ysize*sizeof(short);
	    sbuf = (short *)malloc(bytes);
	    if(!sbuf) {
		fprintf(stderr,"iflip: can't malloc %d bytes\n",bytes);
		exit(1);
	    }
	}
    }
    for(z=0; z<zsize; z++) {
	if(trans) {
	    if(bpp == 1) {
		cptr = cbuf;
		for(y=0; y<ysize; y++) {
		    getfliprow(iimage,row,y,z,flipcode);
		    stoc(row,cptr,xsize);
		    cptr += xsize;
		}
		for(x=0; x<xsize; x++) {
		    cptr = cbuf+x;
		    rptr = row;
		    for(y=ysize; y--;) {
			*rptr++ = *cptr;
			cptr += xsize;
		    }
		    putrow(oimage,row,x,z);
		}
	    } else {
		sptr = sbuf;
		for(y=0; y<ysize; y++) {
		    getfliprow(iimage,sptr,y,z,flipcode);
		    sptr += xsize;
		}
		for(x=0; x<xsize; x++) {
		    sptr = sbuf+x;
		    rptr = row;
		    for(y=ysize; y--;) {
			*rptr++ = *sptr;
			sptr += xsize;
		    }
		    putrow(oimage,row,x,z);
		}
	    }
	} else {
	    for(y=0; y<ysize; y++) {
		getfliprow(iimage,row,y,z,flipcode);
		putrow(oimage,row,y,z);
	    }
	}
    }
    iclose(oimage);
    exit(0);
}
