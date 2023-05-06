/*
 *	subimg - 
 *		Extract a region from an image.
 *
 *				Paul Haeberli - 1984
 *
 */
#include "image.h"

short row[8192];

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *iimage, *oimage;
    register int iy, iz, oy;
    int xsize, ysize;
    int temp;
    int xmin, xmax;
    int ymin, ymax;

    if( argc<7 ) {
	fprintf(stderr,
		"usage: subimg inimage outimage x1 x2 y1 y2\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"subimg: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xmin = atoi(argv[3]);
    xmax = atoi(argv[4]);
    ymin = atoi(argv[5]);
    ymax = atoi(argv[6]);
    xmin = negmap(xmin,iimage->xsize);
    xmax = negmap(xmax,iimage->xsize);
    ymin = negmap(ymin,iimage->ysize);
    ymax = negmap(ymax,iimage->ysize);
    if(xmin>xmax) {
	temp = xmax;
	xmax = xmin;
	xmin = temp;
    }
    if(ymin>ymax) {
	temp = ymax;
	ymax = ymin;
	ymin = temp;
    }
    xsize = xmax-xmin+1;
    ysize = ymax-ymin+1;
    if(xsize>iimage->xsize) {
	fprintf(stderr,"subimg: xsize too big\n");
	exit(1);
    }
    if(ysize>iimage->ysize) {
	fprintf(stderr,"subimg: ysize too big\n");
	exit(1);
    }
    oimage = iopen(argv[2],"w",iimage->type,iimage->dim,
					xsize,ysize,iimage->zsize); 
    isetname(oimage,iimage->name);
    oimage->colormap = iimage->colormap;
    for(iz=0; iz<iimage->zsize; iz++) {
	oy = 0;
	for(iy=ymin; iy<=ymax; iy++) {
	    getrow(iimage,row,iy,iz);
	    putrow(oimage,row+xmin,oy++,iz);
	}
    }
    iclose(oimage);
    exit(0);
}

negmap(val,size)
int val, size;
{
    if(val<0)
	return size-1+val;
    else
	return val;
}
