/*
 *	imgexp - 
 *		Expand the range of pixel values in an image.
 *
 * 			    Paul Haeberli - 1985
 */
#include "image.h"

short buf[8192];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage;
    unsigned int xsize, ysize, zsize;
    unsigned int y, z;
    int i, min[4], max[4];

    if( argc<3 ) {
	fprintf(stderr,"usage: imgexp inimage outimage [[min max] gmin gmax bmin bmax]\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"imgexp: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    min[0] = iimage->min;
    max[0] = iimage->max;
    if(argc>=5) {
	min[0] = atoi(argv[3]);
	max[0] = atoi(argv[4]);
    } else if(argc==4) {
	min[0] = 0;
	max[0] = atoi(argv[3]);
    }
    if(argc>=9) {
	min[1] = atoi(argv[5]);
	max[1] = atoi(argv[6]);
	min[2] = atoi(argv[7]);
	max[2] = atoi(argv[8]);
    } else {
	min[1] = min[0];
	max[1] = max[0];
	min[2] = min[0];
	max[2] = max[0];
    }
    min[3] = min[0];
    max[3] = max[0];
    for(i=0; i<4; i++) {
	if(max[i]==min[i])
	    max[i] = min[i]+1;
    }
    oimage = iopen(argv[2],"w",RLE(1),iimage->dim,xsize,ysize,iimage->zsize); 
    oimage->colormap = iimage->colormap;
    for(z=0; z<zsize; z++) {
	for(y=0; y<ysize; y++) {
	    getrow(iimage,buf,y,z);
	    imgexp(buf,xsize,min[z],max[z]-min[z]);
	    putrow(oimage,buf,y,z);
	}
    }
    iclose(oimage);
    exit(0);
}

imgexp(sptr,n,min,delta)
register short *sptr;
register int n, min;
register int delta;
{
    register int val;

    while(n--) {
	val = *sptr-min;
	if(val<0)
	    val = 0;
	val = (val*255)/delta;
	if(val>255)
	    val = 255;
	*sptr++ = val;
    }
}
