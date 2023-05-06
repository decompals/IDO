/*
 *	mapimg - 
 *		Use a color map to transform a screen image into an rgb image.
 *
 *				Paul Haeberli - 1985	
 */
#include "image.h"

short	rbuf[8192];
short	gbuf[8192];
short	bbuf[8192];
short	abuf[8192];
short	ibuf[8192];
short 	rmap[4096];
short 	gmap[4096];
short 	bmap[4096];
short 	rowbuf[10];

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage, *mapfile;
    unsigned int xsize, ysize;
    unsigned int y;

    if(argc<4 ) {
	fprintf(stderr,"usage: mapimg inimage.sc outimage.rgb file.map\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"mapimg: can't open input file %s\n",argv[1]);
	exit(0);
    }
    if( (mapfile=iopen(argv[3],"r")) == NULL ) {
	fprintf(stderr,"mapimg: can't open input file %s\n",argv[3]);
	exit(0);
    }
    readmap(mapfile,rmap,gmap,bmap);
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    oimage = iopen(argv[2],"w",RLE(1),3,xsize,ysize,3); 
    for(y=0; y<ysize; y++) {
	getrow(iimage,ibuf,y,0);
	domap(ibuf,rbuf,gbuf,bbuf,abuf,xsize);
	putrow(oimage,rbuf,y,0);
	putrow(oimage,gbuf,y,1);
	putrow(oimage,bbuf,y,2);
    }
    iclose(oimage);
    exit(0);
}

domap(ibuf,rbuf,gbuf,bbuf,abuf,n)
register unsigned short *ibuf, *rbuf, *gbuf, *bbuf, *abuf;
register int n;
{
    while(n--) { 
	*rbuf++ = rmap[*ibuf];
	*gbuf++ = gmap[*ibuf];
	*bbuf++ = bmap[*ibuf++];
	*abuf++ = 255;
    }
}

readmap(mapfile,rmap,gmap,bmap)
register IMAGE *mapfile;
register unsigned short *rmap, *gmap, *bmap;
{
    register int i, index;

    if(mapfile->xsize != 4) {
	fprintf(stderr,"readmap: wierd map file!! \n");
	exit(1);
    }
    for(i=0; i<4096; i++) {
	rmap[i] = 0;
	gmap[i] = 0;
	bmap[i] = 0;
    }
    for(i=0; i<mapfile->ysize; i++) {
	getrow(mapfile,rowbuf,i,0);
	index = rowbuf[0];
	if(index > 4096) {
	    fprintf(stderr,"readmap: index way out there!!\n");
	    exit(1);
	}
	rmap[index] = rowbuf[1];
	gmap[index] = rowbuf[2];
	bmap[index] = rowbuf[3];
    }
}
