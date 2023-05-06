/*
 *	toascii - 
 *		Convert an Iris image to ascii characters.
 *
 *				Paul Haeberli - 1990
 */
#include "image.h"

short row0[8192]; 
short row1[8192]; 

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *image;
    int xsize;
    int i, y, ysize;

    if( argc<2 ) {
	fprintf(stderr,"usage: toascii inimage.bw\n");
	exit(1);
    } 
    if( (image=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"tobin: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = image->xsize;
    ysize = image->ysize;
    ysize -= (ysize&1);
    for(y=0; y<ysize; y+=2) {
	getrow(image,row0,ysize-1-(y+0),0);
	getrow(image,row1,ysize-1-(y+1),0);
	asciirow(row0,row1,xsize);
    }
    exit(0);
}

char tab[6][6] = {
    { '$', '$', 'b', 'b', 'm', 'm' },
    { '$', '1', 'b', 'b', 'm', 'm' },
    { 'P', 'P', '|', '|', ',', ',' },
    { 'P', 'P', '|', ':', ',', ',' },
    { '"', '"', '`', '`', '.', ' ' },
    { '"', '"', '`', '`', ' ', ' ' },
};

asciirow(buf0,buf1,n)
short *buf0, *buf1;
int n;
{
    int c0 , c1;

    while(n--) {
        c0 = ((*buf0++)*6)/256;
        c1 = ((*buf1++)*6)/256;
	printf("%c",tab[c1][c0]);
    }
    printf("\n");
}
