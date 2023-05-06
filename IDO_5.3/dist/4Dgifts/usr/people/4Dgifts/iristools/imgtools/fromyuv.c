/*
 *	fromyuv - 
 *		Convert an Abekas yuv image file into an iris image.
 *
 *				Paul Haeberli - 1989
 *
 */
#include "stdio.h"
#include "image.h"

#define NROWS		243
#define YSIZE		(2*NROWS)
#define BYTESPERROW	1440
#define XSIZE		720
#define	DESTXSIZE	640

#define LMARGIN		0
#define RMARGIN		0
#define IMGXSIZE	(XSIZE-LMARGIN-RMARGIN)
#define IMGYSIZE	(YSIZE)

unsigned short rbuf[1024];
unsigned short gbuf[1024];
unsigned short bbuf[1024];
unsigned short obuf[1024];

main(argc,argv)
char	**argv;
{
    int	fd, square;
    IMAGE *image;

    if(argc < 3) {
	fprintf(stderr,"usage: fromyuv in.yuv out.rgb [-s]\n");
	exit(1);
    }
    if(argc>3) 
	square = 1;
    else
	square = 0;
    if((fd = open(argv[1],0)) < 0) {
	fprintf(stderr,"fromyuv: error: cannot open %s\n",argv[1]);
	exit(1);
    }
    if(square) 
	image = iopen(argv[2],"w",RLE(1),3,DESTXSIZE,IMGYSIZE,3);
    else
	image = iopen(argv[2],"w",RLE(1),3,IMGXSIZE,IMGYSIZE,3);
    yuvtorgb(fd,image,square);
    iclose(image);
    exit(0);
}

readframe(fd,buf)
int fd;
unsigned char *buf;
{
    int n;

    n = read(fd,buf,2*NROWS*BYTESPERROW);
    if(n != (2*NROWS*BYTESPERROW)) {
	fprintf(stderr,"fromyuv: read error %d\n",n);
	exit(1);
    }
}

yuvtorgb(fd,image,square)
int fd;
IMAGE *image;
int square;
{
    int	y;
    unsigned char *ap;
    unsigned char *picbuf;

    picbuf = (unsigned char *)malloc(2*NROWS*BYTESPERROW);
    readframe(fd,picbuf);
    ap = picbuf;
    for(y = 0; y < YSIZE; y++) {
	UYVYtoRGB(ap,rbuf,gbuf,bbuf);
	ap += BYTESPERROW;
 	if(square) {
	    zoom720to640(rbuf,obuf);
	    putrow(image,obuf,(YSIZE-1)-y,0);
	    zoom720to640(gbuf,obuf);
	    putrow(image,obuf,(YSIZE-1)-y,1);
	    zoom720to640(bbuf,obuf);
	    putrow(image,obuf,(YSIZE-1)-y,2);
	} else {
	    putrow(image,rbuf,(YSIZE-1)-y,0);
	    putrow(image,gbuf,(YSIZE-1)-y,1);
	    putrow(image,bbuf,(YSIZE-1)-y,2);
	}
    }
}
