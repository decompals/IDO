/*
 *	fromcube -
 *		Convert a Cubicomp/Vertigo Image to an Iris image.
 *
 *				Paul Haeberli - 1989
 */
#include "image.h"

char rbuf[4096];
char gbuf[4096];
char bbuf[4096];

typedef struct hdr {
     short xsize;
     short ysize;
     short dummy1;
     short dummy2;
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
	fprintf(stderr,"usage: fromcube cubicomp.pic outimage.rgb\n");
	exit(1);
    }
    if ((infile = fopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"fromcube: can't open %s\n",argv[1]);
	exit(1);
    }
    fread(&ihdr,sizeof(ihdr),1,infile);
    xsize = ihdr.xsize;
    ysize = ihdr.ysize;
    image = iopen(argv[2],"w",RLE(1),3,xsize,ysize,3);
    for(y=0; y<ysize; y++) {
	getcc(infile,rbuf,gbuf,bbuf,xsize);
	putrow(image,rbuf,ysize-1-y,0);
	putrow(image,gbuf,ysize-1-y,1);
	putrow(image,bbuf,ysize-1-y,2);
    }
    iclose(image);
    exit(0);
}

getcc(infile,rbuf,gbuf,bbuf,xsize)
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
