/*
 *	repcol - 
 *		Replace a color in an image.
 *
 *				Paul Haeberli - 1989
 */
#include "image.h"
#include "stdio.h"
#include "math.h"

short rbuf[8192];
short gbuf[8192];
short bbuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *iimage, *oimage;
    register int y, z;
    int xsize, ysize, zsize;
    int ir, ig, ib;
    int or, og, ob;
    int dist;

    if( argc<10 ) {
	fprintf(stderr,"usage: repcol inimage.rgb outimage.rgb ir ig ib or og ob dist\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"repcol: can't open input file %s\n",argv[1]);
	exit(1);
    }
    if (iimage->zsize < 3) {
	fprintf(stderr,"repcol: works only on color images\n");
	exit(1);
    }
    ir = atoi(argv[3]);
    ig = atoi(argv[4]);
    ib = atoi(argv[5]);
    or = atoi(argv[6]);
    og = atoi(argv[7]);
    ob = atoi(argv[8]);
    dist = atoi(argv[9]);
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    if(zsize>3)
	zsize = 3;
    oimage = iopen(argv[2],"w",RLE(1),3,xsize,ysize,zsize); 
    for(y=0; y<ysize; y++) {
	getrow(iimage,rbuf,y,0);
	getrow(iimage,gbuf,y,1);
	getrow(iimage,bbuf,y,2);
	repcol(rbuf,gbuf,bbuf,xsize,ir,ig,ib,or,og,ob,dist);
	putrow(oimage,rbuf,y,0);
	putrow(oimage,gbuf,y,1);
	putrow(oimage,bbuf,y,2);
    }
    iclose(oimage);
    exit(0);
}

repcol(rbuf,gbuf,bbuf,n,ir,ig,ib,or,og,ob,dist)
short *rbuf;
short *gbuf;
short *bbuf;
int n;
int ir, ig, ib;
int or, og, ob;
int dist;
{
    while(n--)  {
	if(colordist(*rbuf,*gbuf,*bbuf,ir,ig,ib) <= dist) {
	    *rbuf = or;
	    *gbuf = og;
	    *bbuf = ob;
	}
	rbuf++;
	gbuf++;
	bbuf++;
    }
}

colordist(r0,g0,b0,r1,g1,b1)
int r0,g0,b0,r1,g1,b1;
{
    register int dist, dr, dg, db;

    dr = r1-r0;
    dg = g1-g0;
    db = b1-b0;
    dist = 0;
    if(dr<0) 
	dist -= dr;
    else
	dist += dr;
    if(dg<0) 
	dist -= dg;
    else 
	dist += dg;
    if(db<0) 
	dist -= db;
    else 
	dist += db;
    return dist;
}
