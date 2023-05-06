/*
 *	setlum - 
 *		Set the luminence of an rgb image using a b/w image.
 *
 *				Paul Haeberli - 1988
 */
#include "port.h"
#include "image.h"
#include "vect.h"
#include "lum.h"

short rbuf[8192];
short gbuf[8192];
short bbuf[8192];
short ibuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage1, *iimage2, *oimage;
    unsigned int xsize, ysize, zsize;
    unsigned int y, z;

    if( argc<4 ) {
	fprintf(stderr,"usage: setlum inimage1 inimage2 outimage\n");
	exit(0);
    } 
    if( (iimage1=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"setlum: can't open input file %s\n",argv[1]);
	exit(1);
    }
    if( (iimage2=iopen(argv[2],"r")) == NULL ) {
	fprintf(stderr,"setlum: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = MIN(iimage1->xsize,iimage2->xsize);
    ysize = MIN(iimage1->ysize,iimage2->ysize);
    zsize = iimage1->xsize;
    oimage = iopen(argv[3],"w",RLE(1),iimage1->dim,xsize,ysize,iimage1->zsize); 
    for(y=0; y<ysize; y++) {
	getrow(iimage1,rbuf,y,0);
	getrow(iimage1,gbuf,y,1);
	getrow(iimage1,bbuf,y,2);
	getrow(iimage2,ibuf,y,0);
	setlum(rbuf,gbuf,bbuf,ibuf,xsize);
	putrow(oimage,rbuf,y,0);
	putrow(oimage,gbuf,y,1);
	putrow(oimage,bbuf,y,2);
    }
    iclose(oimage);
    exit(0);
}

setlum(rbuf,gbuf,bbuf,ibuf,n)
register unsigned short *rbuf, *gbuf, *bbuf, *ibuf;
register int n;
{
    int x;

    for(x=0; x<n; x++) 
	dosetlum(rbuf+x,gbuf+x,bbuf+x,ibuf+x);
}

dosetlum(r,g,b,i)
unsigned short *r, *g, *b, *i;
{
    register int lum, wantlum;
    register int whiteness;
    register int colorness;
    register int div;

    noblack(r,g,b);
    lum = ILUM(*r,*g,*b);
    wantlum = *i;
    if(wantlum<=lum) {
	if(lum>0) {
	    *r = ((*r) * wantlum)/lum;
	    *g = ((*g) * wantlum)/lum;
	    *b = ((*b) * wantlum)/lum;
	} else {
	    *r = 0;
	    *g = 0;
	    *b = 0;
	}
    } else {
	colorness = 255-wantlum;
	whiteness = 255*(wantlum-lum);
	div = 255-lum;
	*r = ((*r)*colorness + whiteness)/div;
	*g = ((*g)*colorness + whiteness)/div;
	*b = ((*b)*colorness + whiteness)/div;
    }
}

noblack(r,g,b)
unsigned short *r, *g, *b;
{
    int i;

    i = *r;
    if(*g>i)
	i = *g;
    if(*b>i)
	i = *b;
    if(i == 255)
	return;
    if(i>0) {
	*r = (255* *r)/i;
	*g = (255* *g)/i;
	*b = (255* *b)/i;
    } else {
	*r = 255;
	*g = 255;
	*b = 255;
    }
}
