/*
 *	saturate - 
 *		Change the saturation on an RGB image.
 *
 *				Paul Haeberli - 1988
 */
#include "image.h"
#include "lum.h"
#include "math.h"

short rbuf[8192];
short gbuf[8192];
short bbuf[8192];
int carefull = 0;

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage;
    unsigned int xsize, ysize, zsize;
    unsigned int y, z;
    float p;

    if( argc<4 ) {
	fprintf(stderr,"usage: saturate inimage1 outimage sat [-c]\n");
	exit(0);
    } 
    if(argc>4)
	carefull++;
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"setlum: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    oimage = iopen(argv[2],"w",RLE(1),3,xsize,ysize,3); 
    p = atof(argv[3]);
    for(y=0; y<ysize; y++) {
	getrow(iimage,rbuf,y,0%zsize);
	getrow(iimage,gbuf,y,1%zsize);
	getrow(iimage,bbuf,y,2%zsize);
	if(carefull)
	    carefullsaturate(rbuf,gbuf,bbuf,xsize,p);
	else
	    saturate(rbuf,gbuf,bbuf,xsize,p);
	putrow(oimage,rbuf,y,0);
	putrow(oimage,gbuf,y,1);
	putrow(oimage,bbuf,y,2);
    }
    iclose(oimage);
    exit(0);
}

carefullsaturate(rbuf,gbuf,bbuf,n,p)
register unsigned short *rbuf, *gbuf, *bbuf;
register int n;
float p;
{
    int bw, x;
    int r, g, b;
    int dr, dg, db;
    float puse, pmin, pmax;

    for(x=0; x<n; x++) {
	r = *rbuf;
	g = *gbuf;
	b = *bbuf;
	bw = RINTLUM*r + GINTLUM*g + BINTLUM*b;
	r <<= 8;
	g <<= 8;
	b <<= 8;
	dr = r-bw;
	dg = g-bw;
	db = b-bw;
	pmin = -10000.0;
	pmax = 10000.0;
	minmax(bw,dr,&pmin,&pmax);
	minmax(bw,dg,&pmin,&pmax);
	minmax(bw,db,&pmin,&pmax);
	puse = p;
	if(puse<pmin)
	    puse = pmin;
	if(puse>pmax)
	    puse = pmax;
	r = bw+puse*dr;
	g = bw+puse*dg;
	b = bw+puse*db;
	r >>= 8;
	g >>= 8;
	b >>= 8;
	*rbuf++ = r;
	*gbuf++ = g;
	*bbuf++ = b;
    }
}

minmax(bw,dc,pmin,pmax)
int bw, dc;
float *pmin, *pmax;
{
    float p;

    if(dc>0) {
	p = ((float)(-bw))/dc;
	if(p>*pmin)
	    *pmin = p;
	p = ((float)((255<<8)-bw))/dc;
	if(p<*pmax)
	    *pmax = p;
    } else if(dc<0) {
	p = ((float)(-bw))/dc;
	if(p<*pmax)
	    *pmax = p;
	p = ((float)((255<<8)-bw))/dc;
	if(p>*pmin)
	    *pmin = p;
    }
}

saturate(r,g,b,n,sat)
short *r, *g, *b;
float sat;
{
    int bw;
    int rwgt, gwgt, bwgt;
    int rt, gt, bt;

    rwgt = RINTLUM*(1.0-sat);
    gwgt = GINTLUM*(1.0-sat);
    bwgt = BINTLUM*(1.0-sat);
    while(n--) {
	rt = *r;
	gt = *g;
	bt = *b;
	bw = (rwgt*rt + gwgt*gt + bwgt*bt)>>8;
	rt = bw+sat*rt;
	gt = bw+sat*gt;
	bt = bw+sat*bt;
	if(rt<0) rt = 0; else if(rt>255) rt=255;
	if(gt<0) gt = 0; else if(gt>255) gt=255;
	if(bt<0) bt = 0; else if(bt>255) bt=255;
	*r++ = rt;
	*g++ = gt;
	*b++ = bt;
    }
}
