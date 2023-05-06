/*
 *	istat - 
 *		Print the header of image files.
 *
 *				Paul Haeberli - 1984
 */
#include "image.h"

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *image;
    int i;

    if( argc<2 ) {
	fprintf(stderr,"usage: istat imagefiles . . .\n");
	exit(1);
    } 
    printf("xsize ysize zsize   min   max  bpp  type  storage name\n");
    for(i=1; i<argc; i++) {
	if( (image=iopen(argv[i],"r")) == NULL ) {
	    fprintf(stderr,"istat: can't open input file %s\n",argv[1]);
	    exit(1);
	}
	printf("%5d ",image->xsize);
	printf("%5d ",image->ysize);
	printf("%5d ",image->zsize);
	printf("%5d ",image->min);
	printf("%5d ",image->max);
	printf("  %d  ",BPP(image->type));
	switch(image->colormap) {
	case CM_NORMAL:
	    printf("NORMAL  ");
		break;
	case CM_DITHERED:
	    printf("DITHERED");
		break;
	case CM_SCREEN:
	    printf("SCREEN  ");
		break;
	case CM_COLORMAP:
	    printf("COLORMAP");
		break;
	default:
	    printf("%8x", image->colormap);
		break;
	}
	if(ISRLE(image->type)) 
	    printf("rle    ");
	else
	    printf("verb   ");
  	printname(argv[i]);
	printf("\n");
	iclose(image);
    }
    exit(0);
}

printname(s)
char *s; 
{
    char *cptr, *lcptr;

    cptr = s;
    lcptr = 0;
    while(*cptr) {
	if(*cptr == '/')
	    lcptr = cptr;
	cptr++;
    }
    if(lcptr)
	printf("%s",lcptr+1);
    else
	printf("%s",s);
}
