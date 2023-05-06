/*
 *	tomac -
 *		Convert an IRIS image file to macpaint format.
 *
 *				Paul Haeberli - 1989
 *
 */
#include "image.h"

#define HEADERSIZE	512

#define MAXXSIZE	576
#define MAXYSIZE	720

short sbuf[8192];
unsigned char ibits[MAXXSIZE/8+20];
unsigned char pbits[MAXXSIZE/8+20];
char header[HEADERSIZE];

main(argc,argv) 
int argc;
char *argv[];
{ 
    if(argc<3) {
	fprintf(stderr,"usage: tomac image.bw image.mac\n");
	exit(1);
    }
    writemac(argv[1],argv[2]);
    exit(0);
}

writemac(iname,oname)
char *iname, *oname;
{
    FILE *outf;
    IMAGE *iimage;
    int i, y, n;
    int xmargin, ymargin;
    int xsize, ysize;

    iimage = iopen(iname,"r");
    if(!iimage) {
	fprintf(stderr,"tomac: can't open input file %s\n",iname);
	exit(1);
    }
    outf = fopen(oname,"w");
    if(!outf) {
	fprintf(stderr,"tomac: can't open output file %s\n",iname);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    xmargin = (MAXXSIZE-xsize)/2.0;
    if(xmargin<0)
	xmargin = 0;
    ymargin = (MAXYSIZE-ysize)/2.0;
    if(ymargin<0)
	ymargin = 0;
    for (i=0; i<HEADERSIZE; i++)
	fputc(0,outf);
    setrow(sbuf,sbuf,255,MAXXSIZE);
    for(y=0; y<MAXYSIZE; y++) {
 	if(y>ymargin && y<(ymargin+ysize)) 
	    getrow(iimage,sbuf+xmargin,ysize-1-(y-ymargin),0);
 	else
	    setrow(sbuf,255,MAXXSIZE);
	rowtobits(sbuf,ibits,MAXXSIZE);
	n = packbits(ibits,pbits,MAXXSIZE);
  	fwrite(pbits,n,1,outf);
    }
    iclose(iimage);
    fclose(outf);
    return 1;
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
    return optr - pbits;
}
