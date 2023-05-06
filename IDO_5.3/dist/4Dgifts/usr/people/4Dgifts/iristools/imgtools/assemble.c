/*
 *	assemble - 
 *		Assemble an nx by ny array of smaller images.
 *
 *				Paul Haeberli - 1986
 */
#include "image.h"

#define XMAJOR	0x4
#define YMAJOR	0x0

#define LEFT	0x0
#define RIGHT	0x2

#define BOTTOM	0x0
#define TOP	0x1

short row[8192];

int nx, ny;
int order;
int inimages;

char *findname(argc,argv,x,y,order)
int argc;
char **argv;
int x, y, order;
{
    int p;

    if(order&RIGHT)
	x = nx-1-x;
    if(order&TOP)
	y = ny-1-y;
    if(order&XMAJOR)
	p = y*nx+x;
    else
	p = x*ny+y;
    p = p%inimages;
    return argv[4+p];
}

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
    char *iname;

    if(argc < 5) {
	fprintf(stderr,"usage: assemble nx ny outimage imgfiles . . .\n");
	exit(1);
    } 
    nx = atoi(argv[1]);
    ny = atoi(argv[2]);
    order = XMAJOR+BOTTOM+LEFT;
    oimage = 0;
    firsted = 0;
    inimageno = 0;
    inimages = argc-4;
    for(y=0; y<ny; y++) {
	for(x=0; x<nx; x++) {
	    iname = findname(argc,argv,x,y,order);
	    iimage[x] = iopen(iname,"r");
	    if(!iimage[x]) {
		printf("assemble: can't open input file %s\n",iname);
		exit(1);
	    }
	    inimageno++;
	    if(!firsted) {
		xsize = iimage[0]->xsize;
		ysize = iimage[0]->ysize;
		zsize = iimage[0]->zsize;
		firsted = 1;
	    } else if(iimage[x]->xsize != xsize || iimage[x]->ysize!=ysize) {
		printf("assemble: size mismatch!\n");
		exit(1);
	    }
	}
	if(!oimage) {
	    oimage = iopen(argv[3],"w",RLE(BPP(iimage[0]->type)),iimage[0]->dim,nx*xsize,ny*ysize,zsize);
	}
	for(z=0; z<zsize; z++) {
	    for(line=0; line<ysize; line++) {
		for(x=0; x<nx; x++) 
		    getrow(iimage[x],row+x*xsize,line,z%iimage[x]->zsize);
		putrow(oimage,row,y*ysize+line,z);
	    }
	}
	for(x=0; x<nx; x++) 
	    iclose(iimage[x]);
    }
    iclose(oimage);
    exit(0);
}

