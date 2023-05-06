/*
 *	imean - 
 *		Find the average pixel value of an image.
 *
 *				Paul Haeberli - 1989
 */
#include "image.h"
#include "lum.h"
#include "math.h"

short sbuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage;
    int rc, gc, bc;
    int xsize, ysize, zsize;
    int y;

    if( argc<2 ) {
	fprintf(stderr,"usage: imean inimage\n");
	exit(1);
    } 
    iimage=iopen(argv[1],"r");
    if(!iimage) {
	fprintf(stderr,"imean: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    rc = gc = bc = 0;
    for(y=0; y<ysize; y++) {
	if(zsize<3) {
	    getrow(iimage,sbuf,y,0);
	    rc += tallyrow(sbuf,xsize);
	} else {
	    getrow(iimage,sbuf,y,0);
	    rc += tallyrow(sbuf,xsize);
	    getrow(iimage,sbuf,y,1);
	    gc += tallyrow(sbuf,xsize);
	    getrow(iimage,sbuf,y,2);
	    bc += tallyrow(sbuf,xsize);
	}
    }
    rc = rc/(xsize*ysize);
    gc = gc/(xsize*ysize);
    bc = bc/(xsize*ysize);
    if(zsize<3) 
	printf("%d\n",rc);
    else 
	printf("%d %d %d\n",rc,gc,bc);
    exit(0);
}

tallyrow(buf,n)
short *buf;
int n;
{
    int c;

    c = 0;
    while(n--) 
	c += *buf++;
    return c;
}
