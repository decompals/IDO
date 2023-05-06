/*
 *	makecard - 
 *		Make in image into a postcard.
 *
 *				Paul Haeberli - 1988
 */
#include "image.h"

#define CARDXINCH	4.25
#define CARDYINCH	6.00
#define BORDER		0.15
#define LINEWIDTH	0.01

main(argc,argv)
int argc;
char **argv;
{
    float aspect;
    int xsize, ysize, zsize;
    int cardxsize, cardysize;
    int imgxorg, imgyorg;
    int linepixels;
    char oneline[256];
    IMAGE *iimage, *oimage;
    unsigned int y, oy;
    int i;

    if( argc<3 ) {
	fprintf(stderr,"usage: makecard inimage outimage\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"makecard: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    iclose(iimage);
    aspect = ((float)xsize)/ysize;
    if(aspect >= (CARDXINCH-2*BORDER)/(CARDYINCH-2*BORDER)) {
        cardxsize = (xsize*CARDXINCH)/(CARDXINCH-2*BORDER);
        cardysize = (cardxsize*CARDYINCH)/CARDXINCH;
        imgxorg = (cardxsize*BORDER)/CARDXINCH;
        imgyorg = cardysize-imgxorg-ysize;
        imgyorg = (cardysize-ysize)/2;
    } else {
        cardysize = (ysize*CARDYINCH)/(CARDYINCH-2*BORDER);
        cardxsize = (cardysize*CARDXINCH)/CARDYINCH;
        imgyorg = (cardxsize*BORDER)/CARDXINCH;
        imgxorg = (cardxsize-xsize)/2;
    }
    linepixels = (LINEWIDTH*cardxsize)/CARDXINCH;
    if(linepixels<1)
	linepixels = 1;
    sprintf(oneline,"nullimg PSblack.rgb %d %d %d",
			     xsize+2*linepixels,ysize+2*linepixels,zsize);
    system(oneline);
    sprintf(oneline,"nullimg PScard.rgb %d %d %d",cardxsize,cardysize,zsize);
    system(oneline);
    system("invert PScard.rgb PSt.rgb");
    system("mv PSt.rgb PScard.rgb");
    sprintf(oneline,"over PScard.rgb PSblack.rgb PSt.rgb %d %d",
				  imgxorg-linepixels,imgyorg-linepixels);
    system(oneline);
    system("rm PSblack.rgb");
    system("mv PSt.rgb PScard.rgb");
    sprintf(oneline,"over PScard.rgb %s PSt.rgb %d %d",argv[1],imgxorg,imgyorg);
    system(oneline);
    system("rm PScard.rgb"); 
    sprintf(oneline,"addframe PSt.rgb %s",argv[2]);
    system(oneline);
    system("rm PSt.rgb");
    exit(0);
}
