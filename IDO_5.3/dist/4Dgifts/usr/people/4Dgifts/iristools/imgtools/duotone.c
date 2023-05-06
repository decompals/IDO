/*
 *	duotone - 
 *		Create a duotone from a black and white image.
 *
 *				Paul Haeberli - 1988
 */
#include "image.h"
#include "math.h"
#include "lum.h"

short irbuf[8192];
short igbuf[8192];
short ibbuf[8192];
short rbuf[8192];
short gbuf[8192];
short bbuf[8192];

short rtab[256];
short gtab[256];
short btab[256];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage;
    unsigned int xsize, ysize, zsize;
    unsigned int y, z;
    int r, g, b, l;
    float p;

    if( argc<6 ) {
	fprintf(stderr,"usage: duotone inimage.bw outimage.rgb r g b\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"duotone: can't open input file %s\n",argv[1]);
	exit(1);
    }
    r = atoi(argv[3]);
    g = atoi(argv[4]);
    b = atoi(argv[5]);
    l = ILUM(r,g,b);
    r = (r*128)/l;
    g = (g*128)/l;
    b = (b*128)/l;
    maketabs(r,g,b);
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    oimage = iopen(argv[2],"w",RLE(1),3,xsize,ysize,3); 
    for(y=0; y<ysize; y++) {
	if(zsize<3) {
	    getrow(iimage,irbuf,y,0);
	    duotone(irbuf,irbuf,irbuf,rbuf,gbuf,bbuf,xsize);
	} else {
	    getrow(iimage,irbuf,y,0);
	    getrow(iimage,igbuf,y,1);
	    getrow(iimage,ibbuf,y,2);
	    duotone(irbuf,igbuf,ibbuf,rbuf,gbuf,bbuf,xsize);
	}
	putrow(oimage,rbuf,y,0);
	putrow(oimage,gbuf,y,1);
	putrow(oimage,bbuf,y,2);
    }
    iclose(oimage);
    exit(0);
}

maketabs(r,g,b)
int r, g, b;
{
    mkcurve(r/255.0,rtab);
    mkcurve(g/255.0,gtab);
    mkcurve(b/255.0,btab);
}

duotone(irbuf,igbuf,ibbuf,rbuf,gbuf,bbuf,n)
register unsigned short *irbuf, *igbuf, *ibbuf;
register unsigned short *rbuf, *gbuf, *bbuf;
register int n;
{
    int x;

    for(x=0; x<n; x++) {
	*rbuf++ = rtab[*irbuf++];
	*gbuf++ = gtab[*igbuf++];
	*bbuf++ = btab[*ibbuf++];
    }
}

float logbase(base,f)
float base, f;
{
    return log(f)/log(base);
}

mkcurve(f,tab)
float f;
short tab[256];
{
    int i;
    float igamma;

    igamma = logbase(0.5,f);
    for(i=0; i<256; i++) 
	tab[i] = 255.0*pow(i/255.0,igamma)+0.5;
}
