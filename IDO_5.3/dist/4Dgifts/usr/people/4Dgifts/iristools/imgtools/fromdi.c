/*
 *	frombin -
 *		Convert an old .di dithered image into an RGB image.
 *
 *				Paul Haeberli - 1988
 */
#include "image.h"

short sbuf[8192];
short rbuf[8192];
short gbuf[8192];
short bbuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage;
    int y, xsize, ysize, zsize;

    if(argc<3) {
	fprintf(stderr,"usage: fromdi inimage.di outimage.rgb\n");
	exit(1);
    }
    iimage = iopen(argv[1],"r");
    if(!iimage) {
	fprintf(stderr,"fromdi: can't open %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    oimage = iopen(argv[2],"w",RLE(1),3,xsize,ysize,3);
    for(y=0; y<ysize; y++) {
	getrow(iimage,sbuf,y,0);
	fromdi(sbuf,rbuf,gbuf,bbuf,xsize);
	putrow(oimage,rbuf,y,0);
	putrow(oimage,gbuf,y,1);
	putrow(oimage,bbuf,y,2);
    }
    iclose(iimage);
    iclose(oimage);
    exit(0);
}

short rmap[256];
short gmap[256];
short bmap[256];

fromdi(sbuf,rbuf,gbuf,bbuf,n)
short *sbuf, *rbuf, *gbuf, *bbuf;
int n;
{
    static int firsted;
    int i, r, g, b;

    if(!firsted) {
	for (i=0; i<256; i++) {
	    r = (i>>0) & 7;
	    g = (i>>3) & 7;
	    b = (i>>6) & 3;
	    r = (255*r)/7;
	    g = (255*g)/7;
	    b = (255*b)/3;
	    rmap[i] = r;
	    gmap[i] = g;
	    bmap[i] = b;
    	}
	firsted = 1;
    }
    while(n--) {
	*rbuf++ = rmap[*sbuf&0xff];
	*gbuf++ = gmap[*sbuf&0xff];
	*bbuf++ = bmap[*sbuf&0xff];
	sbuf++;
    }
}
