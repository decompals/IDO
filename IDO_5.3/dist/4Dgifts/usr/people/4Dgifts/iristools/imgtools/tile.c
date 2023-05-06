/*
 *	tile -
 *		Repeat an image in two dimensions.
 *
 *				Paul Haeberli - 1988
 */
#include "image.h"

short row[8192];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage;
    IMAGE *oimage;
    int ixsize, iysize, izsize;
    int oxsize, oysize, ozsize;
    int x, y, z;

    if( argc < 5 ) {
	printf("usage: tile inimage outimage xsize ysize\n");
	exit(1);
    } 
    iimage = iopen(argv[1],"r");
    if(!iimage) {
	printf("tile: can't open input file %s\n",argv[1]);
	exit(1);
    } 
    ixsize = iimage->xsize;
    iysize = iimage->ysize;
    izsize = iimage->zsize;
    oxsize = atoi(argv[3]);
    oysize = atoi(argv[4]);
    ozsize = izsize;
    oimage = iopen(argv[2],"w",iimage->type,iimage->dim,oxsize,oysize,ozsize);
    for(z=0; z<ozsize; z++) {
	for(y=0; y<oysize; y++) {
	    getrow(iimage,row,y%iysize,z);
	    reprow(row,ixsize,oxsize);
	    putrow(oimage,row,y,z);
	}
    }
    iclose(oimage);
    exit(0);
}

reprow(buf,isize,osize)
short *buf;
int isize, osize;
{
    register short *iptr, *optr;
    register int n, togo;

    optr = buf+isize;
    togo = osize-isize;
    while (togo>0) {
	if (togo>isize) 
	    n = isize;
	else
	    n = togo; 
 	iptr = buf;
	togo -= n;
	while(n--) 
	    *optr++ = *iptr++;
    }
}
