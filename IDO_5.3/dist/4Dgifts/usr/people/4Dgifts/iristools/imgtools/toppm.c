/*
 *	toppm -
 *		Convert an IRIS image file into Jef Poskanzer's ppm
 *	image format.
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
 *
 */
#include "image.h"
#include "stdio.h"

#define MAXIWIDTH	8192

short rbuf[MAXIWIDTH];
short gbuf[MAXIWIDTH];
short bbuf[MAXIWIDTH];
unsigned char cbuf[3*MAXIWIDTH];

main(argc,argv)
int argc;
char **argv;
{
    if(argc<3) {
	fprintf(stderr,"usage: toppm inimage.rgb outimage.ppm\n");
	exit(1);
    }
    toppm(argv[1],argv[2],32);
    exit(0);
}

toppm(iname,ppmname)
char *iname, *ppmname;
{
    FILE *of;
    IMAGE *image;
    int xsize, ysize, zsize;
    int x, y;
    unsigned char *cptr;

    image = iopen(iname,"r");
    if(!image) {
	fprintf(stderr,"toppm: can't open input file %s\n",iname);
	exit(1);
    }
    of = fopen(ppmname,"w");
    if(!of) {
	fprintf(stderr,"toppm: can't open output file %s\n",ppmname);
	exit(1);
    }
    xsize = image->xsize;
    ysize = image->ysize;
    zsize = image->zsize;

    fprintf(of,"P6\n");
    fprintf(of,"# IRIS image file 24 bit rgb\n");
    fprintf(of,"%d %d\n",xsize,ysize);
    fprintf(of,"255\n");
    for(y=0; y<ysize; y++) {
	if(zsize<3) {
	    getrow(image,rbuf,ysize-1-y,0);
	    cptr = cbuf;
	    for(x=0; x<xsize; x++) {
		*cptr++ = rbuf[x];
		*cptr++ = rbuf[x];
		*cptr++ = rbuf[x];
	    }
	    fwrite(cbuf,3*xsize,1,of);
	} else {
	    getrow(image,rbuf,ysize-1-y,0);
	    getrow(image,gbuf,ysize-1-y,1);
	    getrow(image,bbuf,ysize-1-y,2);
	    cptr = cbuf;
	    for(x=0; x<xsize; x++) {
		*cptr++ = rbuf[x];
		*cptr++ = gbuf[x];
		*cptr++ = bbuf[x];
	    }
	    fwrite(cbuf,3*xsize,1,of);
	}
    }
    fclose(of);
    iclose(image);
}
