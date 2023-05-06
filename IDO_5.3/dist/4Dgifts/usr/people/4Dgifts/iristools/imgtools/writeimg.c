/*
 *	writeimg -
 *		Write out an RGB image file.  This example uses three functions
 *	from the image library: 
 *
 *		iopen, putrow, and iclose.	
 *
 * 	The function iopen is called to describe the xsize and ysize of the 
 *	RGB image to be written out.  
 *
 *	The function putrow writes a row of image data to the image file. It is
 *	called with an array of shorts with values in the range [0..255].
 *	
 *	The function iclose is called to close the image file.
 *
 *	Why not modify this program to be a filter that converts from your own
 *	image file format to IRIS images? 
 *
 *				Paul Haeberli - 1987
 *
 */
#include "image.h"

unsigned short rbuf[8192];
unsigned short gbuf[8192];
unsigned short bbuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    int y;
    int xsize, ysize;
    IMAGE *image;

    if(argc<4) {
	fprintf(stderr,"usage writeimg name xsize ysize\n");
	exit(1);
    }
    xsize = atoi(argv[2]);
    ysize = atoi(argv[3]);
    image = iopen(argv[1],"w",RLE(1),3,xsize,ysize,3);
    for(y=0; y<ysize; y++) {
	/*
		fill rbuf, gbuf, and bbuf with pixel values 
	*/
	putrow(image,rbuf,y,0);		/* red row */
	putrow(image,gbuf,y,1);		/* green row */
	putrow(image,bbuf,y,2);		/* blue row */
    }
    iclose(image);
    exit(0);
}
