/*
 *	tobin - 
 *		Convert an Iris image to binary dump format.
 *
 *				Paul Haeberli - 1986
 */
#include "image.h"

short rowbuf[8192]; 
char charbuf[8192]; 

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *image;
    FILE *outf;
    int xsize;
    int i, y, ysize;
    int z, zsize;
    int outshorts;

    if( argc<2 ) {
	fprintf(stderr,"usage: tobin inimage out.bin [-s]\n");
	exit(1);
    } 
    if(argc>3)
	outshorts = 1;
    else
	outshorts = 0;
    if( (image=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"tobin: can't open input file %s\n",argv[1]);
	exit(1);
    }
    if( (outf=fopen(argv[2],"w")) == NULL ) {
	fprintf(stderr,"tobin: can't open output file %s\n",argv[1]);
	exit(1);
    }
    xsize = image->xsize;
    ysize = image->ysize;
    zsize = image->zsize;
    if(outshorts) {
	for(y=0; y<ysize; y++) {
	    getrow(image,rowbuf,y,z);
	    fwrite(rowbuf,1,xsize*sizeof(short),outf);
	}
    } else {
	for(z=0; z<zsize; z++) {
	    for(y=0; y<ysize; y++) {
		getrow(image,rowbuf,y,z);
		stoc(rowbuf,charbuf,xsize);
		fwrite(charbuf,1,xsize,outf);
	    }
	}
    }
    fclose(outf);
    exit(0);
}
