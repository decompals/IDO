/*
 *	fromalias -
 *		Convert an Alias image to an Iris image.
 *
 *				Paul Haeberli - 1986
 */
#include "image.h"

short rbuf[8192];
short gbuf[8192];
short bbuf[8192];

typedef struct hdr {
     short xsize;
     short ysize;
     short xorg;
     short yorg;
     short bitsdeep;
} hdr;

hdr ihdr;

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *image;
    FILE *infile;
    int xsize, ysize;
    int y, z;

    if(argc<3) {
	fprintf(stderr,"usage: fromalias aliasimage outimage.rgb\n");
	exit(1);
    }
    if ((infile = fopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"fromalias: can't open %s\n",argv[1]);
	exit(1);
    }
    fread(&ihdr,sizeof(ihdr),1,infile);
    xsize = ihdr.xsize;
    ysize = ihdr.ysize;
    image = iopen(argv[2],"w",RLE(1),3,xsize,ysize,3);
    for(y=0; y<ysize; y++) {
	getalias(infile,rbuf,gbuf,bbuf,xsize);
	putrow(image,rbuf,ysize-1-y,0);
	putrow(image,gbuf,ysize-1-y,1);
	putrow(image,bbuf,ysize-1-y,2);
    }
    iclose(image);
    exit(0);
}

getalias(infile,rbuf,gbuf,bbuf,xsize)
FILE *infile;
register unsigned short *rbuf, *gbuf, *bbuf;
int xsize;
{
    register int n, count;
    register int r, g, b;
    unsigned long val;

    for(n=0; n<xsize;) {
	fread(&val,sizeof(long),1,infile);
	r =  val&0xff;
	val >>= 8;
	g =  val&0xff;
	val >>= 8;
	b =  val&0xff;
	val >>= 8;
	count =  val&0xff;
	n += count;
	while(count--) {
	    *rbuf++ = r;
	    *gbuf++ = g;
	    *bbuf++ = b;
	}
    }
}
