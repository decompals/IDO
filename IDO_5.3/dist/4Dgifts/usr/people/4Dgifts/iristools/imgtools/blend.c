/*
 *	blend - 
 *		Blend two images.
 *
 *				Paul Haebetli - 1987
 */
#include "port.h"
#include "image.h"
#include "stdio.h"
#include "math.h"

short rowbuf1[8192];
short rowbuf2[8192];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage1, *iimage2, *oimage;
    int y, z;
    int xsize, ysize, zsize1, zsize2;
    int ozsize;
    float p;

    if( argc<5 ) {
	fprintf(stderr,"usage: blend inimage1 inimage2 outimage param\n");
	exit(1);
    } 
    if( (iimage1=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"blend: can't open input file %s\n",argv[1]);
	exit(1);
    }
    if( (iimage2=iopen(argv[2],"r")) == NULL ) {
	fprintf(stderr,"blend: can't open input file %s\n",argv[2]);
	exit(1);
    }
    xsize = MIN(iimage1->xsize,iimage2->xsize);
    ysize = MIN(iimage1->ysize,iimage2->ysize);
    zsize1 = iimage1->zsize;
    zsize2 = iimage2->zsize;
    ozsize = MAX(zsize1,zsize2);
    oimage = iopen(argv[3],"w",RLE(BPP(iimage1->type)),
					iimage1->dim,xsize,ysize,ozsize); 
    p = atof(argv[4]);
    isetname(oimage,iimage1->name);
    for(z=0; z<ozsize; z++)
	for(y=0; y<ysize; y++) {
	    getrow(iimage1,rowbuf1,y,z%zsize1);
	    getrow(iimage2,rowbuf2,y,z%zsize2);
	    blendrow(rowbuf1,rowbuf2,xsize,p);
	    putrow(oimage,rowbuf2,y,z);
	}
    iclose(oimage);
    exit(0);
}

blendrow(buf1,buf2,n,p)
short *buf1;
short *buf2;
int n;
float p;
{
    int val; 

    while (n--) {
	val = *buf1 + p*(*buf2-*buf1);
	if(val>255) 
	    val = 255;
	else if(val<0)
	    val = 0;
	buf1++;
	*buf2++ = val;
    }
}
