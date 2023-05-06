/*
 *	blend - 
 *		Blend two images using a mat image.
 *
 *				Paul Haebetli - 1987
 */
#include "port.h"
#include "image.h"
#include "stdio.h"
#include "math.h"

short rowbuf1[8192];
short rowbuf2[8192];
short matbuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *iimage1, *iimage2, *matimage, *oimage;
    register int y, z;
    int xsize, ysize, zsize1, zsize2;
    int ozsize, matzsize;
    float p;

    if( argc<5 ) {
	fprintf(stderr,"usage: iblend inimage1 inimage2 outimage matimg\n");
	exit(1);
    } 
    if( (iimage1=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"iblend: can't open input file %s\n",argv[1]);
	exit(1);
    }
    if( (iimage2=iopen(argv[2],"r")) == NULL ) {
	fprintf(stderr,"iblend: can't open input file %s\n",argv[2]);
	exit(1);
    }
    if( (matimage=iopen(argv[4],"r")) == NULL ) {
	fprintf(stderr,"iblend: can't open input file %s\n",argv[4]);
	exit(1);
    }
    zsize1 = iimage1->zsize;
    zsize2 = iimage2->zsize;
    xsize = MIN(iimage1->xsize,iimage2->xsize);
    ysize = MIN(iimage1->ysize,iimage2->ysize);
    ozsize = MAX(zsize1,zsize2);
    xsize = MIN(xsize,matimage->xsize);
    ysize = MIN(ysize,matimage->ysize);
    matzsize = matimage->zsize;
    oimage = iopen(argv[3],"w",RLE(1),3,xsize,ysize,ozsize); 
    for(z=0; z<ozsize; z++) {
	for(y=0; y<ysize; y++) {
	    getrow(iimage1,rowbuf1,y,z%zsize1);
	    getrow(iimage2,rowbuf2,y,z%zsize2);
	    getrow(matimage,matbuf,y,z%matzsize);
	    blendrow(rowbuf1,rowbuf2,matbuf,xsize);
	    putrow(oimage,rowbuf2,y,z);
	}
    }
    iclose(oimage);
    exit(0);
}

blendrow(buf1,buf2,mbuf,n)
register short *buf1;
register short *buf2;
register short *mbuf;
register int n;
{
    int p1, p2, div; 

    div = 255;
    while (n--) {
	p2 = *mbuf++;
	p1 = 255-p2;
	*buf2 = ((p1 * *buf1)+(p2 * *buf2))/div;
	buf1++;
	buf2++;
    }
}
