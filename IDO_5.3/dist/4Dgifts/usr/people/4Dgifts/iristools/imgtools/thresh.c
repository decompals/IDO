/*
 *	thresh - 
 *		Threshold one image with another.
 *
 *			 	Paul Haeberli - 1987
 */
#include "port.h"
#include "image.h"
#include "stdio.h"
#include "texture.h"

short rowbuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *iimage, *oimage;
    register int y, z;
    int xsize, ysize, zsize;
    TEXTURE *tm;

    if( argc<4 ) {
	fprintf(stderr,"usage: thresh inimage outimage threshimage\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"thresh: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    oimage = iopen(argv[2],"w",RLE(1),iimage->dim,xsize,ysize,zsize); 
    if( (tm=tmopen(argv[3])) == NULL ) {
	fprintf(stderr,"thresh: can't open thresh file %s\n",argv[3]);
	exit(0);
    }
    for(z=0; z<zsize; z++) {
	for(y=0; y<ysize; y++) {
	    getrow(iimage,rowbuf,y,z);
	    threshrow(tm,rowbuf,xsize,y);
	    putrow(oimage,rowbuf,y,z);
	}
    }
    iclose(oimage);
    exit(0);
}

threshrow(tm,sbuf,n,ypos)
TEXTURE *tm;
short *sbuf;
int n, ypos;
{
    int zsize, tmx;
    unsigned char *cdata, *cbase;
    unsigned short *sdata, *sbase;

    ypos = ypos%tm->ysize;
    zsize = tm->zsize;
    if(tm->bpp == 1) {
	cbase = tm->data[ypos];
	cdata = cbase;
	tmx = tm->xsize;
	while (n--) {
	    if(*sbuf >= (255-(*cdata))) 
		*sbuf++ = 255;		
	    else
		*sbuf++ = 0;		
	    cdata += zsize;
	    if(--tmx == 0) {
		tmx = tm->xsize;
		cdata = cbase;
	    }
	}
    } else {
	sdata = (unsigned short *)tm->data[ypos];
	sdata = sbase;
	tmx = tm->xsize;
	while (n--) {
	    if(*sbuf >= (255-(*sdata))) 
		*sbuf++ = 255;		
	    else
		*sbuf++ = 0;		
	    sdata += zsize;
	    if(--tmx == 0) {
		tmx = tm->xsize;
		sdata = sbase;
	    }
	}
    }
}

