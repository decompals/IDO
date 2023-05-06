/*
 *	fromppm -
 *		Convert a ppm file from Jef Pozkanzer's package into an 
 *	IRIS image file.
 *
 *  Here's where to the whole PBMPLUS package:
 *
 * 	PBMPLUS, by Jef Poskanzer.  Comprehensive format conversion and image
 *	manipulation package.  The latest version is always available via
 *	anonymous FTP as expo.lcs.mit.edu:contrib/pbmplus.tar.Z and
 *	ftp.ee.lbl.gov:pbmplus.tar.Z.  The version of 22nov89 (which currently
 *	is still the latest version, except for the one official patch so far)
 *	was posted to comp.sources.misc, and is therefore accessible via mail
 *	to one of the archive servers.  This version is also available in the
 *	X.V11R4 release tape.
 *
 *				Paul Haeberli - 1990
 */
#include "image.h"

short rbuf[8192];
short gbuf[8192];
short bbuf[8192];

/* Magic constants. */
#define PPM_MAGIC1 'P'
#define PPM_MAGIC2 '3'
#define RPPM_MAGIC2 '6'
#define PPM_FORMAT (PPM_MAGIC1 * 256 + PPM_MAGIC2)
#define RPPM_FORMAT (PPM_MAGIC1 * 256 + RPPM_MAGIC2)

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *image;
    FILE *inf;
    int raw, magic, maxval;
    int xsize, ysize;
    int y, z;

    if(argc<3) {
	fprintf(stderr,"usage: fromppm image.ppm outimage.rgb\n");
	exit(1);
    }
    if ((inf = fopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"fromppm: can't open %s\n",argv[1]);
	exit(1);
    }
    magic = readmagicnumber(inf);
    if(magic == PPM_FORMAT)
	raw = 0;
    else if(magic == RPPM_FORMAT)
	raw = 1;
    else {
	fprintf(stderr,"fromppm: bad magic number 0x%x\n",magic);
	exit(1);
    }
    xsize = getint(inf);
    ysize = getint(inf);
    maxval = getint(inf);
    image = iopen(argv[2],"w",RLE(1),3,xsize,ysize,3);
    for(y=0; y<ysize; y++) {
	getppm(inf,rbuf,gbuf,bbuf,xsize,1);
	putrow(image,rbuf,ysize-1-y,0);
	putrow(image,gbuf,ysize-1-y,1);
	putrow(image,bbuf,ysize-1-y,2);
    }
    iclose(image);
    exit(0);
}

getppm(inf,rbuf,gbuf,bbuf,n,raw)
FILE *inf;
unsigned short *rbuf, *gbuf, *bbuf;
int n, raw;
{
    if(raw) {
	while(n--) {
	    *rbuf++ = tgetc(inf);
	    *gbuf++ = tgetc(inf);
	    *bbuf++ = tgetc(inf);
	}
    } else {
	while(n--) {
	    *rbuf++ = getint(inf);
	    *gbuf++ = getint(inf);
	    *bbuf++ = getint(inf);
	}
    }
}

readmagicnumber(inf)
FILE *inf;
{
    int c0, c1;

    c0 = tgetc(inf);
    c1 = tgetc(inf);
    return (c0<<8)+c1;
}

getint(inf)
FILE *inf;
{
    register char ch;
    register int i;

    do {
	ch = pbmgetc( inf );
    } while ( ch == ' ' || ch == '\t' || ch == '\n' );
    if ( ch < '0' || ch > '9' )
	fprintf(stderr,"junk in file where an integer should be\n");
    i = 0;
    do {
	i = i * 10 + ch - '0';
	ch = pbmgetc( inf );
    } while ( ch >= '0' && ch <= '9' );
    return i;
}

pbmgetc( inf )
FILE *inf;
{
    int ch;

    ch = tgetc(inf);
    if (ch == '#') {
	do {
	    ch = tgetc(inf);
	} while (ch != '\n');
    }
    return ch;
}

tgetc(inf)
FILE *inf;
{
    int c;

    c = getc(inf);
    if(c == EOF)
	fprintf(stderr,"premature EOF\n");
    return c;
}
