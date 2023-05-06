/*
 *	over - 
 *		Put one image over another image.
 *
 *				Paul Haeberli - 1987
 */
#include "port.h"
#include "image.h"
#include "stdio.h"

short rowbuf1[8192];
short rowbuf2[8192];
short alpha[8192];

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *iimage1, *iimage2, *oimage;
    register int y, z;
    int xsize1, ysize1, zsize;
    int xsize2, ysize2;
    int ozsize;
    int xorg, yorg;
    int ysrc;

    if( argc<6 ) {
	fprintf(stderr,"usage: over underimage overimage outimage xpos ypos\n");
	exit(1);
    } 
    if( (iimage1=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"add: can't open input file %s\n",argv[1]);
	exit(1);
    }
    if( (iimage2=iopen(argv[2],"r")) == NULL ) {
	fprintf(stderr,"add: can't open input file %s\n",argv[2]);
	exit(1);
    }
    xsize1 = iimage1->xsize;
    ysize1 = iimage1->ysize;
    xsize2 = iimage2->xsize;
    ysize2 = iimage2->ysize;
    zsize = iimage2->zsize;
    xorg = atoi(argv[4]);
    yorg = atoi(argv[5]);
    ozsize = iimage1->zsize;
    if(ozsize>3)
	ozsize = 3;
    oimage = iopen(argv[3],"w",RLE(BPP(iimage1->type)),
					iimage1->dim,xsize1,ysize1,ozsize); 
    isetname(oimage,iimage1->name);
    for(z=0; z<ozsize; z++)
	for(y=0; y<ysize1; y++) {
	    getrow(iimage1,rowbuf1,y,z);
	    ysrc = y-yorg;
	    if(ysrc>=0 && ysrc<iimage2->ysize) {
		if(zsize>3) {
		    getrow(iimage2,rowbuf2,ysrc,z);
		    getrow(iimage2,alpha,ysrc,3);
		    alphaoverrow(rowbuf1,xsize1,rowbuf2,xsize2,xorg,alpha);
	 	} else {
		    if(zsize == 1)
			getrow(iimage2,rowbuf2,ysrc,0);
		    else
			getrow(iimage2,rowbuf2,ysrc,z);
		    overrow(rowbuf1,xsize1,rowbuf2,xsize2,xorg); 
		}
	    }
	    putrow(oimage,rowbuf1,y,z);
	}
    iclose(oimage);
    exit(0);
}

overrow(buf1,n1,buf2,n2,xorg)
register short *buf1;
int n1;
register short *buf2;
int n2;
int xorg;
{
    if((n2+xorg)>n1) 
	n2 = n1-xorg; 
    buf1+=xorg;
    while (n2--) 
	*buf1++ = *buf2++;
}

hackrow(buf1,n1,buf2,n2,xorg)
register short *buf1;
int n1;
register short *buf2;
int n2;
int xorg;
{
    if((n2+xorg)>n1) 
	n2 = n1-xorg; 
    buf1+=xorg;
    while (n2--)  {
	*buf1 = (*buf1 * *buf2)/255;
	buf1++;
	buf2++;
    }
}

alphaoverrow(buf1,n1,buf2,n2,xorg,alpha)
register short *buf1;
int n1;
register short *buf2;
int n2;
int xorg;
register short *alpha;
{
    register int a, val;

    if((n2+xorg)>n1) 
	n2 = n1-xorg; 
    buf1+=xorg;
    while (n2--) {
	a = *alpha++;
	if(a >= 255) {
	    *buf1++ = *buf2++;
	} else if(a == 0) {
	    buf1++;
	    buf2++;
	} else {
	    val = *buf2 + ((255-a) * *buf1)/255;
	    if(val>255) 
		*buf1 = 255;
	    else
		*buf1 = val;
	    buf1++;
	    buf2++;
	}
    }
}
