/* 
 *	totarga - 
 *		Convert from an IRIS image to a a type 2 ( RGB ) targa 
 *	image.  Most targa images are displayed directly on monitors with 
 *	no gamma correction.  The typical gamma is about 2.2, so you 
 *	gotta gammawarp the input image by 0.454545 to get it out of the
 *	linear intensity space, and into monitor space.
 *
 *	 	    		Paul Haeberli - 1988
 */
#include "image.h"
#include "stdio.h"
#include "targa.h"

main(argc,argv)
int argc;
char **argv;
{
    if(argc<3) {
	fprintf(stderr,"usage: totarga inimage.rgb outimage.tga [-32] \n");
	exit(1);
    }
    if(argc>3)
	totarga(argv[1],argv[2],32);
    else
	totarga(argv[1],argv[2],24);
    exit(0);
}

#define MAXIWIDTH	8192

short rbuf[MAXIWIDTH];
short gbuf[MAXIWIDTH];
short bbuf[MAXIWIDTH];

totarga(iname,tname,bits)
char *iname, *tname;
int bits;
{
    int xsize, ysize, zsize;
    int y;
    TARGA t;
    FILE *of;
    IMAGE *image;

    if(bits != 24 && bits != 32) {
	fprintf(stderr,"totarga: bits can only be 24 or 32\n");
	exit(1);
    }
    image = iopen(iname,"r");
    if(!image) {
	fprintf(stderr,"totarga: can't open input file %s\n",iname);
	exit(1);
    }
    of = fopen(tname,"w");
    if(!of) {
	fprintf(stderr,"totarga: can't open output file %s\n",tname);
	exit(1);
    }
    xsize = image->xsize;
    ysize = image->ysize;
    zsize = image->zsize;

    t.numid = 0;
    t.maptyp = 0;
    t.imgtyp = 2;
    t.maporig = 0;
    t.mapsize = 0;
    t.mapbits = 0;
    t.xorig = 0;
    t.yorig = 0;
    t.xsize = xsize;
    t.ysize = ysize;
    t.pixsize = bits;
    t.imgdes = 0;

    outchar(of,t.numid);
    outchar(of,t.maptyp);
    outchar(of,t.imgtyp);
    outshort(of,t.maporig);
    outshort(of,t.mapsize);
    outchar(of,t.mapbits);
    outshort(of,t.xorig);
    outshort(of,t.yorig);
    outshort(of,t.xsize);
    outshort(of,t.ysize);
    outchar(of,t.pixsize);
    outchar(of,t.imgdes);
    for(y=0; y<ysize; y++) {
	if(zsize<3) {
	    getrow(image,rbuf,y,0);
	    puttargarow(of,rbuf,rbuf,rbuf,xsize,bits);
	} else {
	    getrow(image,rbuf,y,0);
	    getrow(image,gbuf,y,1);
	    getrow(image,bbuf,y,2);
	    puttargarow(of,rbuf,gbuf,bbuf,xsize,bits);
	}
    }
    fclose(of);
    iclose(image);
}

outchar(of,c)
FILE *of;
char c;
{
    fputc(c,of);
}

outshort(of,s)
FILE *of;
short s;
{
    fputc((s&0xff),of);
    fputc(((s>>8)&0xff),of);
}

puttargarow(of,r,g,b,n,bits)
register FILE *of;
register short *r, *g, *b;
register int n, bits;
{
    if(bits == 32) {
	while(n--) {
	    fputc(*b,of);
	    fputc(*g,of);
	    fputc(*r,of);
	    fputc(0,of);
	    r++;
	    g++;
	    b++;
	}
    } else {
	while(n--) {
	    fputc(*b,of);
	    fputc(*g,of);
	    fputc(*r,of);
	    r++;
	    g++;
	    b++;
	}
    }
}
