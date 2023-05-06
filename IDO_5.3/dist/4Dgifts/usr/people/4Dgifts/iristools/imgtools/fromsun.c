/*
 *	fromsun.c -
 *		convert any sun image file to Iris format.  
 *
 *	This program should handle 1-bit, 8-bit and 24-bit sun rasterfiles. 
 *
 *			Paul Haeberli - 1989
 *
 */
#include "image.h"
#include "rasterfile.h"

#define MAXWIDTH	8192

char cbuf[4*MAXWIDTH];
short rbuf[MAXWIDTH];
short gbuf[MAXWIDTH];
short bbuf[MAXWIDTH];
unsigned char rmap[256];
unsigned char gmap[256];
unsigned char bmap[256];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *image;
    FILE *inf;
    int xsize, ysize, zsize, rowbytes;
    int y, depth, maplen, rastype, raslength;
    struct rasterfile hdr;
    unsigned char *fb, *fbptr;
    int fblength;

    if(argc<3) {
	fprintf(stderr,"usage: fromsun sunimage irisimage\n");
	exit(1);
    }
    inf = fopen(argv[1],"r");
    if(!inf) {
	fprintf(stderr,"fromsun: can't open %s\n",argv[1]);
	exit(1);
    }
    fread(&hdr,1,sizeof(hdr),inf);
    hdr.ras_magic = RAS_MAGIC;
    xsize = hdr.ras_width;
    ysize = hdr.ras_height;
    depth = hdr.ras_depth;
    rastype = hdr.ras_type;
    rowbytes = hdr.ras_length;
    maplen = hdr.ras_maplength;
    raslength = hdr.ras_length;

    if(depth != 8 && depth != 24 && depth != 1) {
	fprintf(stderr,"fromsun: bad ras_depth is %d\n",hdr.ras_depth);
	exit(1);
    }
    if(maplen == 0 && depth == 8) {
	fprintf(stderr,"fromsun: no map on 8 bit image\n");
	exit(1);
    }
    if(maplen > 0) {
	fread(rmap,maplen/3,1,inf);
	fread(gmap,maplen/3,1,inf);
	fread(bmap,maplen/3,1,inf);
    }

    switch(rastype) {
	case RT_OLD:
	    rowbytes = linebytes(xsize,depth);
	    hdr.ras_length = ysize*rowbytes;
	    break;
	case RT_STANDARD:
	    rowbytes = hdr.ras_length/ysize;
	    break;
	case RT_BYTE_ENCODED:
	    rowbytes = linebytes(xsize,depth);
	    break;
	default:
	    fprintf(stderr,"fromsun: bad ras_type is %d\n",rastype);
	    exit(1);
	    break;
    }
    if(depth == 1)
	image = iopen(argv[2],"w",RLE(1),2,xsize,ysize,1);
    else
	image = iopen(argv[2],"w",RLE(1),3,xsize,ysize,3);
    if(rastype == RT_BYTE_ENCODED) {
	fblength = rowbytes*ysize;
	fb = (unsigned char *)malloc(fblength);
	getrledata(inf,raslength,fb,fblength);
	fbptr = fb;
	for(y=0; y<ysize; y++) {
	    switch(depth) {
		case 1:
		    bitstorow(fbptr,rbuf,xsize);
		    fbptr += rowbytes;
		    putrow(image,rbuf,(ysize-1)-y,0);
		    break;
		case 8:
		    cvtrow8(fbptr,rbuf,gbuf,bbuf,xsize);
		    fbptr += rowbytes;
		    putrow(image,rbuf,(ysize-1)-y,0);
		    putrow(image,gbuf,(ysize-1)-y,1);
		    putrow(image,bbuf,(ysize-1)-y,2);
		    break;
		case 24: 
		    unswizzle24(fbptr,rbuf,gbuf,bbuf,xsize);
		    fbptr += rowbytes;
		    putrow(image,rbuf,(ysize-1)-y,0);
		    putrow(image,gbuf,(ysize-1)-y,1);
		    putrow(image,bbuf,(ysize-1)-y,2);
		    break;
		case 32: 
		    unswizzle32(fbptr,rbuf,gbuf,bbuf,xsize);
		    fbptr += rowbytes;
		    putrow(image,rbuf,(ysize-1)-y,0);
		    putrow(image,gbuf,(ysize-1)-y,1);
		    putrow(image,bbuf,(ysize-1)-y,2);
		    break;
	    }
	}
    } else {
	for(y=0; y<ysize; y++) {
	    switch(depth) {
		case 1:
		    fread(cbuf,rowbytes,1,inf);
		    bitstorow(cbuf,rbuf,xsize);
		    putrow(image,rbuf,(ysize-1)-y,0);
		    break;
		case 8:
		    fread(cbuf,rowbytes,1,inf);
		    cvtrow8(cbuf,rbuf,gbuf,bbuf,xsize);
		    putrow(image,rbuf,(ysize-1)-y,0);
		    putrow(image,gbuf,(ysize-1)-y,1);
		    putrow(image,bbuf,(ysize-1)-y,2);
		    break;
		case 24: 
		    fread(cbuf,rowbytes,1,inf);
		    unswizzle24(cbuf,rbuf,gbuf,bbuf,xsize);
		    putrow(image,rbuf,(ysize-1)-y,0);
		    putrow(image,gbuf,(ysize-1)-y,1);
		    putrow(image,bbuf,(ysize-1)-y,2);
		    break;
		case 32: 
		    fread(cbuf,rowbytes,1,inf);
		    unswizzle32(cbuf,rbuf,gbuf,bbuf,xsize);
		    putrow(image,rbuf,(ysize-1)-y,0);
		    putrow(image,gbuf,(ysize-1)-y,1);
		    putrow(image,bbuf,(ysize-1)-y,2);
		    break;
	    }
	}
    }
    iclose(image);

    exit(0);
}

cvtrow8(cptr,rptr,gptr,bptr,n)
register unsigned char *cptr;
register unsigned short *rptr, *gptr, *bptr;
register int n;
{
    while(n--) {
	*rptr++ = rmap[*cptr];
	*gptr++ = gmap[*cptr];
	*bptr++ = bmap[*cptr++];
    }
}

unswizzle24(cptr,rptr,gptr,bptr,n)
register unsigned char *cptr;
register unsigned short *rptr, *gptr, *bptr;
register int n;
{
    while(n--) {
	*rptr++ = *cptr++;
	*gptr++ = *cptr++;
	*bptr++ = *cptr++;
    }
}

unswizzle32(cptr,rptr,gptr,bptr,n)
register unsigned char *cptr;
register unsigned short *rptr, *gptr, *bptr;
register int n;
{
    while(n--) {
	cptr++;
	*bptr++ = *cptr++;
	*gptr++ = *cptr++;
	*rptr++ = *cptr++;
    }
}

#define	ESCAPE	(128)

getrledata(inf,rlelength,fb,fblength)
FILE *inf;
int rlelength;
unsigned char *fb;
int fblength;
{
    unsigned char *rledat, *expdat, *end;
    unsigned char c;
    int compcount, n, count, space;

    rledat = fb+(fblength-rlelength);
    expdat = fb;
    end = fb+fblength;
    compcount = rlelength;
    n = fread(rledat,1,rlelength,inf);
    if(n != rlelength) {
	fprintf(stderr,"error on read of RLE data\n");
	exit(1);
    }
    while(compcount) {
	compcount--;
        c = *rledat++;
        if(c == ESCAPE) {
	    if(compcount<=0) {
		fprintf(stderr,"getrledata: bad poop1\n");
		exit(1);
	    }
	    compcount--;
	    count = *rledat++;
	    if(count!=0) {
		if(compcount<=0) {
		    fprintf(stderr,"getrledata: bad poop2\n");
		    exit(1);
		}
		compcount--;
		c = *rledat++;
		count++;
		space = end-expdat;
		if(count>space)
		    count = space;
		while(count--) 
		    *expdat++ = c;
	    } else {
	       *expdat++ = ESCAPE;
	    }
	} else {
	    *expdat++ = c;
	}
    }
}

linebytes(xsize,depth)
int xsize, depth;
{
    return (((xsize*depth)+15) >> 3) & ~1;
}
