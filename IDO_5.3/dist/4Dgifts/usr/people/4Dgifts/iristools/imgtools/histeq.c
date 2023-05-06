/*
 *	histeq - 
 *		Perform histogram equalization on an image file.
 *
 *				Paul Haeberli - 1987
 *
 */
#include "image.h"
#include "hist.h"

unsigned short maptab[256];
unsigned short rowbuf[8192];
int addnoise = 0;

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage;
    int min, max;
    register int y, z, i, val, index;
    register long sum;
    float shade, maxshade;
    int xsize, ysize, zsize;
    histogram *hist;

    if( argc<3 ) {
	fprintf(stderr,"usage: histeq inimage outimage [-r]\n");
	exit(1);
    } 
    if( argc>3 ) 
	addnoise = 1;
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"histeq: can't open input file %s\n",argv[1]);
	exit(1);
    }
    oimage = iopen(argv[2],"w",RLE(1),
    			iimage->dim,iimage->xsize,iimage->ysize,iimage->zsize); 
    isetname(oimage,iimage->name);
    min = iimage->min;
    max = iimage->max;
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    if(max>255) {
	fprintf(stderr,"histeq: max may not exceed 255\n");
	exit(1);
    }
    hist = newhist(min,max,256);
    for(y=0; y<ysize; y++) {
	for(z=0; z<zsize; z++) {
	    getrow(iimage,rowbuf,y,z);
	    addtohist(hist,rowbuf,xsize);
	}
    }
    histeqtable(hist,maptab);
    for(y=0; y<ysize; y++) {
	for(z=0; z<zsize; z++) { 
	    getrow(iimage,rowbuf,y,z);
	    applytab(rowbuf,maptab,xsize);
	    putrow(oimage,rowbuf,y,z);
	}
    }
    iclose(oimage);
    exit(0);
}

applytab(buf,tab,n)
register unsigned short *buf, *tab;
register int n;
{
    register int this, next;

    if(addnoise) {
	while(n--) {
	    this = tab[*buf];
	    if(*buf<255)
		next = tab[*buf+1];
	    else
		next = 255;
	    if(next>this) {
		*buf = tab[*buf] + nrand(next-this);
	    } else
		*buf = tab[*buf];
	    buf++;
	}
    } else {
	while(n--) {
	    *buf = tab[*buf];
	    buf++;
	}
    }
}

nrand(n)
int n;
{
    return random()%n;
}
