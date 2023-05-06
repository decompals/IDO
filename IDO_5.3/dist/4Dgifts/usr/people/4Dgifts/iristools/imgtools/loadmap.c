/*
 *	loadmap - 
 *		Load a color map from a file. The color indices and the
 *		color map entries must be in a file written by "savemap".
 *
 *				Paul Haeberli - 1984
 *
 */
#include "image.h"
#include "gl.h"

short rowbuf[20];
short yprev = -1;

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *image;
    register int i, y;
    char *fname, *cp;

    if(argc != 2) {
	fprintf(stderr,"usage: loadmap file\n");
	exit(1);
    }
    fname = argv[1];

    if( (image=iopen(fname, "r")) == NULL ) {
	fprintf(stderr,"loadmap: can't open input file %s\n", fname);
	exit(1);
    }
    if(image->xsize != 4) {
	fprintf(stderr,"loadmap: wierd xsize for map file %d\n",image->xsize);
	exit(1);
    }
    noport();
    foreground();
    winopen("loadmap");

    for(y=0; y<image->ysize; y++) {
	getrow(image,rowbuf,y,0);
	if (rowbuf[0] == yprev)
		break;
	yprev = rowbuf[0];
	gammapcolor(rowbuf[0],rowbuf[1],rowbuf[2],rowbuf[3]);
    }
    iclose(image);
    gflush();
    exit(0);
}
