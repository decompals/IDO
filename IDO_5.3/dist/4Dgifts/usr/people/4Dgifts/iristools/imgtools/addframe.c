/*
 *	addframe - 
 *		Add a black frame around an image.
 *
 *				Paul Haeberli - 1986
 */
#include "image.h"

short	ibuf[8192];
short	obuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    unsigned int xsize, ysize, zsize;
    unsigned int oxsize, oysize;
    IMAGE *iimage, *oimage;
    unsigned int y, oy, z;
    int i, width;
    int rgb[4];

    if( argc<3 ) {
	fprintf(stderr,"usage: addframe inimage outimage [width] [r g b]\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"%s: can't open input file %s\n",argv[0],argv[1]);
	exit(1);
    }
    if(argc>3) 
	width = atoi(argv[3]);
    else
	width = 1;
    if(argc>6) {
	rgb[0] = atoi(argv[4]);
	rgb[1] = atoi(argv[5]);
	rgb[2] = atoi(argv[6]);
	rgb[3] = 0;
    } else {
	rgb[0] = 0;
	rgb[1] = 0;
	rgb[2] = 0;
	rgb[3] = 0;
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    oxsize = xsize+2*width;
    oysize = ysize+2*width;
    oimage = iopen(argv[2],"w",RLE(1),3,oxsize,oysize,zsize); 

    for(z=0; z<zsize; z++) {
	oy = 0;
	clrrow(obuf,oxsize,rgb[z]);
	for(i=0; i<width; i++) 
	    putrow(oimage,obuf,oy++,z);
	for(y=0; y<ysize; y++) {
	    getrow(iimage,ibuf,y,z);
	    framerow(ibuf,obuf,width,xsize,rgb[z]);
	    putrow(oimage,obuf,oy++,z);
	}
	clrrow(obuf,oxsize,rgb[z]);
	for(i=0; i<width; i++) 
	    putrow(oimage,obuf,oy++,z);
    }
    iclose(oimage);
    exit(0);
}

framerow(ibuf,obuf,width,n,val)
register short *ibuf, *obuf;
register int width, n, val;
{
    register int i;
    register short *sptr;

    for(i=0; i<width; i++) 
	*obuf++ = val;
    while(n--) 
	*obuf++ = *ibuf++;
    for(i=0; i<width; i++) 
	*obuf++ = val;
}

clrrow(sptr,n,val)
register short *sptr;
register int n, val;
{
    while(n--) 
	*sptr++ = val;
}
