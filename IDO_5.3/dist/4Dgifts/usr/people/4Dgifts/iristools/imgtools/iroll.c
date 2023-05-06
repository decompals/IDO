/*
 *	iroll - 
 *		Roll an image in x and y.
 *
 *				Paul Haeberli - 1988
 */
#include "image.h"

short ibuf[8192];
short obuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage;
    int xsize, ysize, zsize;
    int y, z;
    int xroll, yroll;

    if( argc<4 ) {
	fprintf(stderr,"usage: iroll inimage outimage xroll yroll\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"%s: can't open input file %s\n",argv[0],argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    oimage = iopen(argv[2],"w",RLE(1),iimage->dim,xsize,ysize,iimage->zsize); 
    xroll = -atoi(argv[3]);
    yroll = -atoi(argv[4]);
    while(xroll<0)
	xroll += xsize;
    while(yroll<0)
	yroll += ysize;
    xroll = xroll % xsize;
    yroll = yroll % ysize;
    for(z=0; z<zsize; z++) {
	for(y=0; y<ysize; y++) {
	    getrow(iimage,ibuf,(y+yroll)%ysize,z);
	    rollrow(ibuf,obuf,xroll,xsize);
	    putrow(oimage,obuf,y,z);
	}
    }
    iclose(oimage);
    exit(0);
}

rollrow(ibuf,obuf,roll,n)
register unsigned short *ibuf, *obuf;
register int roll, n;
{
    bcopy(ibuf+roll,obuf,(n-roll)*sizeof(short));
    bcopy(ibuf,obuf+(n-roll),roll*sizeof(short));
}
