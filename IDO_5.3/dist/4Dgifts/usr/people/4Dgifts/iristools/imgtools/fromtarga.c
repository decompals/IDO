/* 
 *	fromtarga - 
 *		Convert targa image into an IRIS image.  Most targa images are 
 *	displayed directly on monitors with no gamma correction.  The typical 
 *	gamma is about 2.2, so you gotta gammawarp the output image by 2.2 to 
 *	get it into a linear intensity space.
 *
 *	This program supports the following TARGA image types:
 *		Type  2: Uncompressed RGB image.
 *		Type  3: Uncompressed B/W image.
 *		Type 10: Run-Length Encoded RGB image.
 *		Type 11: Run-Length Encoded B/W image.
 *
 *	Type 1 and type 9 images are not supported
 *
 *	 	    		Paul Haeberli - 1988
 */
#include "image.h"
#include "stdio.h"
#include "targa.h"

#define MAXIWIDTH	8192

short rbuf[MAXIWIDTH];
short gbuf[MAXIWIDTH];
short bbuf[MAXIWIDTH];
unsigned char cbuf[4*MAXIWIDTH];
int xsize, ysize, zsize;
int flipcode;
IMAGE *image;

main(argc,argv)
int argc;
char **argv;
{
    if(argc<3) {
	fprintf(stderr,"usage fromtarga inimage.tga outimage.rgb\n");
	exit(1);
    }
    fromtarga(argv[1],argv[2]);
    exit(0);
}

fromtarga(tname,iname)
char *tname, *iname;
{
    int y, mapbytes;
    FILE *inf;
    TARGA t;

    inf = fopen(tname,"r");
    if(!inf) {
	fprintf(stderr,"fromtarga: can't open input file %s\n",tname);
	exit(1);
    }
/* get idlength */
    t.numid = inchar(inf);

/* get map type */
    t.maptyp = inchar(inf);

/* get image type */
    t.imgtyp = inchar(inf);

/* get color map spec */
    t.maporig = inshort(inf);
    t.mapsize = inshort(inf);
    t.mapbits = inchar(inf);

/* get image spec */
    t.xorig = inshort(inf);
    t.yorig = inshort(inf);
    t.xsize = inshort(inf);
    t.ysize = inshort(inf);
    t.pixsize = inchar(inf);

/* get flip code */
    t.imgdes = inchar(inf);
    flipcode = (t.imgdes>>4) & 0x3;
    xsize = t.xsize;
    ysize = t.ysize;

/* skip id data */
    for(y=0; y<t.numid; y++)
	inchar(inf);

/* skip color map data */
    if(t.mapsize>0) {
	switch(t.mapbits) {
	    case 15:
	    case 16:
	   	mapbytes = 2*t.mapsize;
		break;
	    case 24:
	   	mapbytes = 3*t.mapsize;
		break;
	    case 32:
	   	mapbytes = 4*t.mapsize;
		break;
	    default:
		fprintf(stderr,"fromtarga: bad map entry size %d\n",t.mapbits);
		exit(1);
	}
	while(mapbytes--)
	    inchar(inf);
    }
    image = iopen(iname,"w",RLE(1),3,xsize,ysize,3);
    if(!image) {
	fprintf(stderr,"totarga: can't open input file %s\n",iname);
	exit(1);
    }
    switch(t.imgtyp) {
	case 2:
	case 3:
	    readuncompressed(inf,t.pixsize);
	    break;
	case 10:
	case 11:
	    readrle(inf,t.pixsize);
	    break;
  	default:
	    fprintf(stderr,"fromtarga: type is %d, fromtarga only works on type 2, 3, 10 and 11 images only\n",t.imgtyp);
	    exit(1);
    }
    fclose(inf);
    iclose(image);
}

inchar(inf)
FILE *inf;
{
    unsigned char c;

    myfread(inf,&c,1);
    return c;
}

inshort(inf)
FILE *inf;
{
    unsigned char h, l;

    myfread(inf,&l,1);
    myfread(inf,&h,1);
    return (h<<8)+l;
}

readuncompressed(inf,pixsize)
FILE *inf;
int pixsize;
{
    int y;

    for(y=0; y<ysize; y++) {
	gettargadata(inf,rbuf,gbuf,bbuf,xsize,pixsize);
	putfliprow(image,rbuf,y,0,flipcode);
	putfliprow(image,gbuf,y,1,flipcode);
	putfliprow(image,bbuf,y,2,flipcode);
    }
}

gettargadata(inf,r,g,b,n,pixsize)
register FILE *inf;
register short *r, *g, *b;
register int n, pixsize;
{
    unsigned char *cptr;
    unsigned short *sptr;
    short pix;

    switch(pixsize) {
	case 32:
	    myfread(inf,cbuf,4*n);
	    cptr = cbuf;
	    while(n--) {
		*b++ = *cptr++;
		*g++ = *cptr++;
		*r++ = *cptr++;
		cptr++;
	    }
	    break;
	case 24:
	    myfread(inf,cbuf,3*n);
	    cptr = cbuf;
	    while(n--) {
		*b++ = *cptr++;
		*g++ = *cptr++;
		*r++ = *cptr++;
	    }
	    break;
	case 16:
	case 15:
	    while(n--) {
		pix = inshort(inf);
		*r++ = (255*((pix>>10)&0x1f))/0x1f;
		*g++ = (255*((pix>>5)&0x1f))/0x1f;
		*b++ = (255*((pix>>0)&0x1f))/0x1f;
	    }
	    break;
	case 8:
	    myfread(inf,cbuf,n);
	    cptr = cbuf;
	    while(n--) {
		*g++ = *cptr;
		*r++ = *cptr;
		*b++ = *cptr++;
	    }
	    break;
    }
}

printheader(t)
TARGA *t;
{
    fprintf(stderr,"numid %d\n",t->numid);
    fprintf(stderr,"maptype %d\n",t->maptyp);
    fprintf(stderr,"imgtyp %d\n",t->imgtyp);
    fprintf(stderr,"maporig %d mapsize %d \n",t->maporig,t->mapsize);
    fprintf(stderr,"mapbits %d\n",t->mapbits);
    fprintf(stderr,"xorig yorig %d %d\n",t->xorig,t->xorig);
    fprintf(stderr,"xsize ysize %d %d \n",t->xsize,t->ysize);
    fprintf(stderr,"pixsize %d\n",t->pixsize);
    fprintf(stderr,"imgdes %d\n",t->imgdes);
}

int outx, outy;

readrle(inf,pixsize)
FILE *inf;
int pixsize;
{
    int loc, nbpix, ipix;
    short r[1], g[1], b[1];
    unsigned char count;

    outx = 0;
    outy = 0;
    while(outy<ysize) {
	myfread(inf,&count,1);
	loc = count;
	loc &= 0x7f;
	nbpix = loc + 1;		
	if( (count &0x80) != 0x80 ) {
	    for( ipix = 0 ; ipix < nbpix ; ipix++) {
		gettargadata(inf,r,g,b,1,pixsize);
	      	outpixel(r[0],g[0],b[0]);
	    }
	} else {	/* pixels are all the same	*/
	    gettargadata(inf,r,g,b,1,pixsize);
	    for( ipix = 0 ; ipix < nbpix ; ipix++) 
	      	outpixel(r[0],g[0],b[0]);
	}
   }
}

outpixel(r,g,b)
int r, g, b;
{
    rbuf[outx] = r;
    gbuf[outx] = g;
    bbuf[outx] = b;
    outx++;
    if(outx == xsize) {
	putfliprow(image,rbuf,outy,0,flipcode);
	putfliprow(image,gbuf,outy,1,flipcode);
	putfliprow(image,bbuf,outy,2,flipcode);
	outx = 0;
	outy++;
    }
}

myfread(inf,buf,n)
FILE *inf;
char *buf;
int n;
{
    if(!fread(buf,n,1,inf)) 
	 fprintf(stderr,"fromtarga: read error on input\n");
}
