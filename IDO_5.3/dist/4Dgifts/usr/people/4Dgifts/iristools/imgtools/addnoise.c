/*
 *	addnoise - 
 *		Add noise to an image.
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
    IMAGE *iimage, *oimage;
    int y, z;
    int xsize, ysize, zsize;
    float mag;
    TEXTURE *tm;

    if( argc<5 ) {
	fprintf(stderr,"usage: addnoise inimage outimage noiseimage mag\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"addnoise: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    oimage = iopen(argv[2],"w",RLE(1),iimage->dim,xsize,ysize,zsize); 
    if( (tm=tmopen(argv[3])) == NULL ) {
	fprintf(stderr,"addnoise: can't open noise file %s\n",argv[3]);
	exit(0);
    }
    mag = atof(argv[4]);
    isetname(oimage,iimage->name);
    for(z=0; z<zsize; z++)
	for(y=0; y<ysize; y++) {
	    getrow(iimage,rowbuf,y,z);
	    addnoiserow(tm,rowbuf,xsize,y+31*z,(int)(mag*255));
	    putrow(oimage,rowbuf,y,z);
	}
    iclose(oimage);
    exit(0);
}

static int dtab[256];
static int firsted;

addnoiserow(tm,sbuf,n,ypos,mag)
TEXTURE *tm;
short *sbuf;
int n, ypos, mag;
{
    int val, tmx, delta, i, zsize;
    unsigned char *cdata, *cbase;
    unsigned short *sdata, *sbase;

    if(!firsted) {
	for(i=0; i<256; i++) {
	    if(i>=128)
	        dtab[i] = mag*(255-i);
	    else
	        dtab[i] = mag*i;
	}
	firsted = 1;
    }

    ypos = ypos%tm->ysize;
    zsize = tm->zsize;
    if(tm->bpp == 1) {
	cbase = tm->data[ypos];
	cdata = cbase;
	tmx = tm->xsize;
	while (n--) {
	    val = (*cdata)-128;
	    delta = (dtab[*sbuf]*val)/(127*256);
	    *sbuf += delta;
	    sbuf++;
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
	    val = (*sdata)-128;
	    delta = (dtab[*sbuf]*val)/(127*256);
	    *sbuf += delta;
	    sbuf++;
	    sdata += zsize;
	    if(--tmx == 0) {
		tmx = tm->xsize;
		sdata = sbase;
	    }
	}
    }
}

