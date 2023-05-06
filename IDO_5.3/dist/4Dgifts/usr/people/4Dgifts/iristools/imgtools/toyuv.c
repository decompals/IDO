/*
 *	toyuv - 
 *		Convert an iris image into an Abekas .yuv image
 *
 *				Paul Haeberli - 1989
 *
 */
#include "stdio.h"
#include "image.h"

#define NROWS		243
#define YSIZE		(2*NROWS)
#define BYTESPERROW	1440
#define A60XSIZE	720
#define	SQXSIZE		640

unsigned short rbuf[8192];
unsigned short gbuf[8192];
unsigned short bbuf[8192];
unsigned short ibuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    int	square;
    IMAGE *image;
    FILE *outf;

    if(argc < 3) {
	fprintf(stderr,"usage: toyuv in.rgb out.yuv [-s]\n");
	exit(1);
    }
    if(argc>3) 
	square = 1;
    else
	square = 0;
    image = iopen(argv[1],"r");
    if(!image) {
	fprintf(stderr,"toyuv: can't open input file\n");
	exit(1);
    }
    outf = fopen(argv[2],"w"); 
    if(!outf) {
	fprintf(stderr,"toyuv: error: cannot open %s\n",argv[2]);
	exit(1);
    }
    rgbtoyuv(image,outf,square);
    fclose(outf);
    exit(0);
}

writeframe(outf,buf)
FILE *outf;
unsigned char *buf;
{
    int n;

    n = fwrite(buf,1,2*NROWS*BYTESPERROW,outf);
    if(n != (2*NROWS*BYTESPERROW)) {
	fprintf(stderr,"toyuv: write error %d\n",n);
	exit(1);
    }
}

rgbtoyuv(image,outf,square)
IMAGE *image;
FILE *outf;
int square;
{
    int	y, iy, xmarg, ymarg;
    unsigned char *ap;
    unsigned char *picbuf;

    picbuf = (unsigned char *)malloc(2*NROWS*BYTESPERROW);
    ap = picbuf;
    ymarg = (YSIZE-(int)image->ysize)/2;
    if(ymarg<0)
	ymarg = 0;
    if(square) {
	xmarg = (SQXSIZE-(int)image->xsize)/2;
	if(xmarg<0)
	    xmarg = 0;
	for(y = 0; y < YSIZE; y++) {
	    iy = (YSIZE-1)-y-ymarg;
	    if(iy>=0 && iy<image->ysize) {
	    	getrow(image,ibuf+xmarg,iy,0%image->zsize);
	    	zoom640to720(ibuf,rbuf);
	    	getrow(image,ibuf+xmarg,iy,1%image->zsize);
	    	zoom640to720(ibuf,gbuf);
	    	getrow(image,ibuf+xmarg,iy,2%image->zsize);
	    	zoom640to720(ibuf,bbuf);
	    } else {
	    	zerorow(rbuf,A60XSIZE);
	    	zerorow(gbuf,A60XSIZE);
	    	zerorow(bbuf,A60XSIZE);
	    }
	    RGBtoUYVY(rbuf,gbuf,bbuf,ap);
	    ap += BYTESPERROW;
	}
    } else {
	xmarg = (A60XSIZE-image->xsize)/2;
	if(xmarg<0)
	    xmarg = 0;
	for(y = 0; y < YSIZE; y++) {
	    iy = (YSIZE-1)-y-ymarg;
	    if(iy>=0 && iy<image->ysize) {
		getrow(image,rbuf+xmarg,iy,0%image->zsize);
		getrow(image,gbuf+xmarg,iy,1%image->zsize);
		getrow(image,bbuf+xmarg,iy,2%image->zsize);
	    } else {
	    	zerorow(rbuf,A60XSIZE);
	    	zerorow(gbuf,A60XSIZE);
	    	zerorow(bbuf,A60XSIZE);
	    }
	    RGBtoUYVY(rbuf,gbuf,bbuf,ap);
	    ap += BYTESPERROW;
	}
    }
    writeframe(outf,picbuf);
}
