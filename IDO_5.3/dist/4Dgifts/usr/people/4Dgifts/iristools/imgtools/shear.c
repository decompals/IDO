/*
 *	shear - 
 *		Shear an image in the X direction.
 *
 *				Paul Haeberli - 1987
 *
 */
#include "image.h"
#include "math.h"

unsigned short row[8192];
unsigned short outrow[8192];

main(argc,argv)
int argc;
char **argv;
{
    register unsigned long *lptr;
    register unsigned short *sptr;
    register IMAGE *iimage, *oimage;
    register int y, z;
    register int f;
    register float slope, yp;
    int xsize, ysize, zsize;

    if( argc<4 ) {
	fprintf(stderr,"usage: shear inimage outimage slope\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"shear: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    oimage = iopen(argv[2],"w",iimage->type,iimage->dim,
					xsize,ysize,iimage->zsize); 
    slope = atof(argv[3]);
    for(z=0; z<zsize; z++) {
	for(y=0; y<ysize; y++) {
	    yp = y/(ysize-1.0);
	    getrow(iimage,row,y,z);
	    shear(row,outrow,xsize,-slope*(ysize-1.0)*yp);
	    putrow(oimage,outrow,y,z);
	}
    }
    iclose(oimage);
    exit(0);
}

shear(sptr,dptr,n,shift)
register unsigned short *sptr, *dptr;
register int n;
register float shift;
{
    register int i, ishift;
    register unsigned short *ptr;
    register unsigned short *optr;
    float fshift;
    int w0, w1;

    while(shift<0)
	shift += n; 
    while(shift>=n)
	shift -= n; 
    ishift = shift;
    fshift = shift - ishift;
    w0 = (128*fshift)+0.5;
    w1 = 128-w0;

    ptr = sptr+ishift;
    optr = dptr;
    i = n-ishift;
    while(i--) 
	*optr++ = w1 * *ptr++;
    ptr = sptr;
    i = ishift;
    while(i--) 
	*optr++ = w1 * *ptr++;

    ishift++;
    if(ishift == n)
	ishift = 0;
    ptr = sptr+ishift;
    optr = dptr;
    i = n-ishift;
    while(i--) 
	*optr++ += w0 * *ptr++;
    ptr = sptr;
    i = ishift;
    while(i--) 
	*optr++ += w0 * *ptr++;

    optr = dptr;
    while(n--) 
	*optr++ >>= 7;
}
