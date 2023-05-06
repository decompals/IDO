#include "gl/image.h"

unsigned char *
read_bwimage(char *name, int *w, int *h)
{
    unsigned char   *image, *scan;
    IMAGE	    *image_in;
    int		    row, components;

    if ( (image_in = iopen(name, "r")) == NULL) { 
	return 0;
    }

    *w = image_in->xsize;
    *h = image_in->ysize;
    components = image_in->zsize;

    if (components != 1)
	return 0;

    image = (unsigned char *)malloc(sizeof(unsigned char) * *w * *h);

    scan = image;

    for (row = 0; row < *h; row++) {
	short		rowbuff[4096];
	int		i;

	getrow(image_in, rowbuff, row, 0);
	for (i = 0; i < *w; i++)
	    *(scan++) = (unsigned char)rowbuff[i];
    }

    iclose(image_in);    
    return image;
}
