/*
 *	imgsize - 
 *		Print on standard out the x, y and z size of an image.
 *
 *				Paul Haeberli - 1988
 *
 */
#include "image.h"

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *image;

    if( argc<2 ) {
	fprintf(stderr,"usage: imgsize inimage [-2]\n");
	exit(1);
    } 
    image = iopen(argv[1],"r");
    if(!image) {
	fprintf(stderr,"imgsize: can't open input image\n");
	exit(1);
    } 
    if(argc==2)
        printf("%d %d %d\n",image->xsize,image->ysize,image->zsize);
    else
        printf("%d %d\n",image->xsize,image->ysize);
    exit(0);
}
