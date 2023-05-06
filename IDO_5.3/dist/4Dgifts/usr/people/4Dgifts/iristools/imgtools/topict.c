/*
 *	topict - 
 *		Convert an Iris image to APPLE pict format.
 *
 *				Paul Haeberli - 1990
 */
#include "image.h"
#include "pict.h"

short sbuf[8192]; 
char cbuf[8192]; 
char pbuf[8192]; 
int outbytes;
FILE *outf;

putrect(xorg,yorg,xsize,ysize)
int xorg, yorg, xsize, ysize;
{
    putashort(yorg);
    putashort(xorg);
    putashort(ysize);
    putashort(xsize);
}

putfprect(xorg,yorg,xsize,ysize)
int xorg, yorg, xsize, ysize;
{
    putalong(yorg<<16);
    putalong(xorg<<16);
    putalong(ysize<<16);
    putalong(xsize<<16);
}

putalong(l)
long l;
{
    putbyte((l>>24)&0xff);
    putbyte((l>>16)&0xff);
    putbyte((l>>8)&0xff);
    putbyte((l>>0)&0xff);
}

putashort(s)
short s;
{
    putbyte((s>>8)&0xff);
    putbyte((s>>0)&0xff);
}

putbyte(b)
unsigned char b;
{
    char c[1];

    c[0] = b;
    if(!fwrite(c,1,1,outf)) {
	fprintf(stderr,"topict: error on write\n");
	exit(1);
    }
    outbytes++;
}

putbytes(buf,n)
unsigned char *buf;
int n;
{
    if(!fwrite(buf,n,1,outf)) {
	fprintf(stderr,"topict: error on write\n");
	exit(1);
    }
    outbytes+=n;
}

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *image;
    int xsize, ysize, zsize;
    int i, picsize;
    int ssizepos, lsizepos;

    if( argc<3 ) {
	fprintf(stderr,"usage: topict inimage out.pict\n");
	exit(1);
    } 
    if( (image=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"topict: can't open input file %s\n",argv[1]);
	exit(1);
    }
    if( (outf=fopen(argv[2],"w")) == NULL ) {
	fprintf(stderr,"topict: can't open output file %s\n",argv[1]);
	exit(1);
    }
    xsize = image->xsize;
    ysize = image->ysize;
    zsize = image->zsize;
    for(i=0; i<HEADER_SIZE; i++) 
	putbyte(0);
    ssizepos = outbytes;
    putashort(0);	 	/* low 16 bits of file size less HEADER_SIZE */
    putrect(0,0,xsize,ysize);	/* bounding box of picture */
    putashort(PICT_picVersion);
    putashort(0x02ff);		/* version 2 pict */
    putashort(PICT_reservedHeader);	/* reserved header opcode */

    lsizepos = outbytes;
    putalong(0);  		/* full size of the file */
    putfprect(0,0,xsize,ysize);	/* fixed point bounding box of picture */
    putalong(0);  		/* reserved */

    putashort(PICT_clipRgn); 	/* the clip region */
    putashort(10);
    putrect(0,0,xsize,ysize);

    if(zsize<3) 		/* put the image */
	putpict(image,1);
    else 
	putpict(image,0);

    putashort(PICT_EndOfPicture); /* end of pict */

    picsize = outbytes-HEADER_SIZE;
    fseek(outf,ssizepos,0);
    putashort(picsize&0xffff);
    fseek(outf,lsizepos,0);
    putalong(picsize);

    fclose(outf);
    exit(0);
}

putpict(image,bw)
IMAGE *image;
int bw;
{
    int xsize, ysize;
    int y, i, m;
    int nbytes, rowbytes;

    xsize = image->xsize;
    ysize = image->ysize;

    if(bw) {
	putashort(PICT_PackBitsRect); /* 8 bit color mapped */
        rowbytes = xsize;
    } else {
	putashort(PICT_Pack32BitsRect); /* 32 bit rgb */
	rowbytes = 4*xsize;
	putalong(0x000000ff);		/* base address */
    }
    if(rowbytes&1)
	rowbytes++;
    putashort(rowbytes|0x8000);	/* rowbytes */
    putrect(0,0,xsize,ysize);	/* bounds */
    putashort(0);		/* version */

    if(bw)
	putashort(0);	/* packtype */
    else
	putashort(4);	/* packtype */
    putalong(0);	/* packsize */
    putalong(72<<16);	/* hres */
    putalong(72<<16);	/* vres */
    if(bw) {
	putashort(0);	/* pixeltype */
	putashort(8);	/* pixelsize */
	putashort(1);	/* cmpcount */
    } else {
	putashort(16);	/* pixeltype */
	putashort(32);	/* pixelsize */
	putashort(3);	/* cmpcount */
    }
    putashort(8);	/* cmpsize */
    putalong(0);	/* planebytes */
    putalong(0);	/* pmtable */
    putalong(0);	/* pmreserved */

    if(bw) {
	putalong(0);		/* ct seed */
	putashort(0);		/* ct flags */
	putashort(0xff); 	/* color map size */
	for(i=0; i<256; i++) {
	    m = 255-i;
	    putashort(i);	/* index */
	    putashort(m*257);	/* red */
	    putashort(m*257);	/* green */
	    putashort(m*257);	/* blue */
	}
    }
    putrect(0,0,xsize,ysize);	/* scr rect */
    putrect(0,0,xsize,ysize);	/* dest rect */

    if(bw) {
	putashort(0x00);	/* transfer mode */
	for(y=0; y<ysize; y++) {
	    getrow(image,sbuf,ysize-1-y,0);
	    stoc(sbuf,cbuf,xsize);
	    invert(cbuf,xsize);
	    if(rowbytes<8) {
		putbytes(cbuf,rowbytes);
	    } else {
		nbytes = packbits(cbuf,pbuf,8*rowbytes);
		if(rowbytes>250) 
		    putashort(nbytes);
		else
		    putbyte(nbytes);
		putbytes(pbuf,nbytes);
	    }
	}
    } else {
	putashort(0x40);	/* transfer mode */
	for(y=0; y<ysize; y++) {
	    getrow(image,sbuf,ysize-1-y,0);
	    stoc(sbuf,cbuf+0*xsize,xsize);
	    getrow(image,sbuf,ysize-1-y,1);
	    stoc(sbuf,cbuf+1*xsize,xsize);
	    getrow(image,sbuf,ysize-1-y,2);
	    stoc(sbuf,cbuf+2*xsize,xsize);
	    nbytes = packbits(cbuf,pbuf,24*xsize);
	    if(rowbytes>250) 
		putashort(nbytes);
	    else
		putbyte(nbytes);
	    putbytes(pbuf,nbytes);
	}
    }
    if(outbytes&1) 
	putbyte(0);
}

invert(cbuf,n)
unsigned char *cbuf;
int n;
{
    while(n--) {
	*cbuf = 255-(*cbuf);
	cbuf++;
    }
}

packbits(ibits,pbits,nbits)
unsigned char *ibits, *pbits;
int nbits;
{
    int bytes;
    unsigned char *sptr;
    unsigned char *ibitsend;
    unsigned char *optr = pbits;
    int nbytes, todo, cc, count;

    nbytes = ((nbits-1)/8)+1;
    ibitsend = ibits+nbytes;
    while(ibits<ibitsend) {
	sptr = ibits;
	ibits += 2;
	while((ibits<ibitsend)&&((ibits[-2]!=ibits[-1])||(ibits[-1]!=ibits[0])))
	    ibits++;
 	if(ibits != ibitsend) {
	    ibits -= 2;
	}
	count = ibits-sptr;
	while(count) {
	    todo = count>127 ? 127:count;
	    count -= todo;
	    *optr++ = todo-1;
	    while(todo--)
		*optr++ = *sptr++;
	}
	if(ibits == ibitsend)
	    break;
	sptr = ibits;
	cc = *ibits++;
	while( (ibits<ibitsend) && (*ibits == cc) )
	    ibits++;
	count = ibits-sptr;
	while(count) {
	    todo = count>128 ? 128:count;
	    count -= todo;
	    *optr++ = 257-todo;
	    *optr++ = cc;
	}
    }
    return optr-pbits;
}
