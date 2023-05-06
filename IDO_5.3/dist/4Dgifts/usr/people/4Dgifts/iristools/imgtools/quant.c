/*
 *	quant - 
 *		Quantify an image to have n levels.
 *
 *			Paul Haeberli - 1988
 */
#include "image.h"

short	table[256];
short	buf[8192];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage;
    unsigned int xsize, ysize, zsize;
    unsigned int y, z;
    int nlevels;

    if( argc<4 ) {
	fprintf(stderr,"usage: quant inimage outimage nlevels\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"quant: can't open input file %s\n",argv[1]);
	exit(1);
    }
    if(iimage->max>255) {
	fprintf(stderr,"quant: max can't exceed 255\n");
	exit(1);
    }
    nlevels = atoi(argv[3]);
    if(nlevels>256)
	nlevels = 256;
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    oimage = iopen(argv[2],"w",RLE(1),iimage->dim,xsize,ysize,iimage->zsize); 
    makequanttab(table,nlevels);
    for(z=0; z<zsize; z++) {
	for(y=0; y<ysize; y++) {
	    getrow(iimage,buf,y,z);
	    applytab(buf,table,xsize);
	    putrow(oimage,buf,y,z);
	}
    }
    iclose(oimage);
    exit(0);
}

applytab(buf,tab,n)
register short *buf, *tab;
register int n;
{
    while(n--) {
	*buf = tab[*buf];
	buf++;
    }
}

makequanttab(tab,nsteps)
short *tab;
int nsteps;
{
    int stepsize, i, val;
   
    if (nsteps<2) {
	for (i=0; i<256; i++ )
	     tab[i] = 128;
    } else {
	stepsize = 256/nsteps;
	for (i=0; i<256; i++ ) {
	     val = (stepsize/2) + (((i*nsteps)/256)*(255))/(nsteps);
	     tab[i] = val;
	}
    }
}
