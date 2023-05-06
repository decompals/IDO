/*
 *	iavg - 
 *		Average a collection of images
 *
 *				Paul Haeberli - 1989
 */
#include "image.h"

short row[8192];
short acc[8192];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage[1024];
    IMAGE *oimage;
    int xsize, ysize, zsize;
    int x, y, z, line, i;
    int firsted;
    int inimageno;
    int inimages;

    if( argc < 3 ) {
	fprintf(stderr,"usage: iavg outimage imgfiles . . .\n");
	exit(1);
    } 
    oimage = 0;
    firsted = 0;
    inimageno = 0;
    inimages = argc-2;
    for(i=0; i<inimages; i++) {
	iimage[i] = iopen(argv[2+i],"r");
	if(!iimage[i]) {
	    printf("iavg: can't open input file %s\n",argv[2+x]);
	    exit(1);
	}
	inimageno++;
	if(!firsted) {
	    xsize = iimage[0]->xsize;
	    ysize = iimage[0]->ysize;
	    zsize = iimage[0]->zsize;
	    firsted = 1;
	} else if(iimage[i]->xsize != xsize || iimage[i]->ysize!=ysize) {
	    printf("iavg: size mismatch!\n");
	    exit(1);
	}
	if(!oimage) {
	    oimage = iopen(argv[1],"w",RLE(BPP(iimage[0]->type)),iimage[0]->dim,xsize,ysize,zsize);
	}
    }
    for(z=0; z<zsize; z++) {
	for(y=0; y<ysize; y++) {
	    zerorow(acc,xsize);
	    for(i=0; i<inimages; i++) {
		getrow(iimage[i],row,y,z%iimage[i]->zsize);
		addsrow(acc,row,xsize);
	    }
	    mydivrow(acc,inimages,xsize);
	    putrow(oimage,acc,y,z);
	}
    }
    for(i=0; i<inimages; i++) 
	iclose(iimage[i]);
    iclose(oimage);
    exit(0);
}

mydivrow(buf,d,n)
short *buf;
int d, n;
{
    int half;

    half = d/2;
    while(n--) {
	*buf = (*buf+half)/d;
	buf++;
    }
}
