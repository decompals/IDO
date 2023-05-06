/*
 *	tosun - 
 *		Convert an iris image into sun 24 bit format
 *
 *			    Paul Haeberli - 1990
 */
#include "image.h"
#include "rasterfile.h"

#define MAXWIDTH	8192

short r[MAXWIDTH];
short g[MAXWIDTH];
short b[MAXWIDTH];
char outbuf[MAXWIDTH];
struct rasterfile hdr;
FILE *ofile;

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage;
    int y, z;
    int xsize, ysize, zsize;
    int rowbytes;

    if(argc<3) {
	fprintf(stderr,"usage: tosun irisimage outfile.ras\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"rle: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    rowbytes = (3*xsize+1) & ~1; 
    hdr.ras_magic = RAS_MAGIC;
    hdr.ras_width = xsize;
    hdr.ras_height = ysize;
    hdr.ras_depth = 24;
    hdr.ras_length = ysize*rowbytes;
    hdr.ras_type = 1;
    hdr.ras_maptype = RMT_NONE;
    hdr.ras_maplength = 0;
    ofile = fopen(argv[2],"w");
    fwrite(&hdr,sizeof(hdr),1,ofile);
    for(y=0; y<ysize; y++) {
	if(zsize<3) {
	    getrow(iimage,r,(ysize-1)-y,0);
	    outrow(r,r,r,xsize,rowbytes);
	} else {
	    getrow(iimage,r,(ysize-1)-y,0);
	    getrow(iimage,g,(ysize-1)-y,1);
	    getrow(iimage,b,(ysize-1)-y,2);
	    outrow(r,g,b,xsize,rowbytes);
	}
    }
    fclose(ofile);
    exit(0);
}

swizzle24(rptr,gptr,bptr,cptr,n)
register unsigned short *rptr, *gptr, *bptr;
register unsigned char *cptr;
register int n;
{
    while(n--) {
	*cptr++ = *rptr++;
	*cptr++ = *gptr++;
	*cptr++ = *bptr++;
    }
}

outrow(rbuf,gbuf,bbuf,n,rowbytes)
unsigned short *rbuf, *gbuf, *bbuf;
int n, rowbytes;
{
    swizzle24(rbuf,gbuf,bbuf,outbuf,n);
    fwrite(outbuf,rowbytes,1,ofile);
}
