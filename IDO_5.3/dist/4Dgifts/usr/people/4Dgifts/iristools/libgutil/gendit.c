/*
 *	gendit - 
 *		General support for dithering. Slow, but correct.
 *
 *				Paul Haeberli - 1990
 */
#include "stdio.h"
#define DEFDITHER
#include "dither.h"

dither *newdither(mat,xsize,ysize,nlevels)
short *mat;
int xsize, ysize;
int nlevels;
{
    dither *di;
    short *sptr;
    int x, y, nmat;
    int min, max;

    nmat = xsize*ysize;
    di = (dither *)malloc(sizeof(dither));
    di->mat = (short *)malloc(xsize*ysize*sizeof(short));
    sptr = mat;
    min = max = *sptr;
    for(y=0; y<ysize; y++) {
	for(x=0; x<xsize; x++) {
	    if(*sptr<min)
		min = *sptr;
	    if(*sptr>max)
		max = *sptr;
	    sptr++;
	}
    }
    if(min != 0) {
	fprintf(stderr,"newdither: matrix min must be 0\n");
	exit(1);
    }
    bcopy(mat,di->mat,xsize*ysize*sizeof(short));
    di->xsize = xsize;
    di->ysize = ysize;
    di->nmat = max+1;
    di->nlevels = nlevels;
    di->nshades = di->nmat*(nlevels-1)+1;
    di->lut = 0;
    return di;
}

dither *clonedither(di)
dither *di;
{
    dither *cdi;

    cdi = (dither *)malloc(sizeof(di));
    cdi->mat = (short *)malloc(di->xsize*di->ysize*sizeof(short));
    bcopy(di->mat,cdi->mat,di->xsize*di->ysize*sizeof(short));
    cdi->xsize = di->xsize;
    cdi->ysize = di->ysize;
    cdi->nmat = di->nmat;
    cdi->nlevels = di->nlevels;
    cdi->nshades = di->nshades;
    cdi->lut = 0;
    return cdi;
}

freedither(di)
dither *di;
{
    if(di) {
	free(di->mat);
	if(di->lut)
	    free(di->lut);
	free(di);
    }
}

rolldither(di,xroll,yroll)
dither *di;
int xroll, yroll;
{
    int x, y, xs, ys, xsize, ysize;
    short *temp;

    while(xroll<0)
	xroll+=xsize;
    while(yroll<0)
	yroll+=xsize;
    temp = (short *)malloc(di->xsize*di->ysize*sizeof(short));
    xsize = di->xsize;
    ysize = di->ysize;
    for(y=0; y<ysize; y++) {
	for(x=0; x<xsize; x++) {
	    xs = (x+xroll)%xsize;
	    ys = (y+yroll)%ysize;
	    temp[y*xsize+x] = di->mat[ys*xsize+xs];
	}
    }
    bcopy(temp,di->mat,di->xsize*di->ysize*sizeof(short));
    free(temp);
    if(di->lut) {
	free(di->lut);
	di->lut = 0;
    }
}

ditrow(di,buf,y,n)
dither *di;
short *buf;
int y, n;
{
    int val;
    int nshades, nmat, xsize;
    short *mat;

    mat = di->mat+(di->xsize*(y%di->ysize));
    nshades = di->nshades;
    nmat = di->nmat;
    xsize = di->xsize;
    while(n--) {
	val = (nshades*(*buf))/256;
	if((val%nmat)>mat[n%xsize])
	    val = (val/nmat)+1;
	else
	    val = (val/nmat)+0;
	*buf++ = val;
    }
}

ditherexp(di,buf,n)
dither *di;
short *buf;
int n;
{
    int max;

    max= di->nlevels-1;
    if(max == 255)
	return;
    while(n--) {
	*buf = (255*(*buf))/max;
	buf++;
    }
}

ditmerge(rbuf,gbuf,bbuf,obuf,n,base,nr,ng,nb)
short *rbuf, *gbuf, *bbuf, *obuf;
int n, base, nr, ng, nb;
{
    int gmul, bmul;

    gmul = nr;
    bmul = nr*ng;
    while(n--) 
	*obuf++ = base+(*rbuf++)+gmul*(*gbuf++)+bmul*(*bbuf++);
}
