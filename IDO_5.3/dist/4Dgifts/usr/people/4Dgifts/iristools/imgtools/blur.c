/*
 *	blur - 
 *		Blur an image by zooming it down and then up.
 *
 *				Paul Haeberli - 1988
 */
#include "image.h"
#include "math.h"

main(argc,argv)
int argc;
char **argv;
{
    int xsize, ysize, zsize;
    int sxsize, sysize, max;
    IMAGE *iimage, *oimage;
    float radius, xfactor, yfactor;
    float xrad, yrad;
    char cmd[256];

    if( argc<4 ) {
	fprintf(stderr,"usage: blur inimage outimage [radius] [xradius yradius]\n");
	exit(1);
    } 
    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"blur: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    iclose(iimage);
    if(xsize>ysize)
	max = xsize;
    else
	max = ysize;
    if(argc==4) {
	radius = atof(argv[3]);
	radius = radius/max;
	sxsize = (xsize*radius)+0.5;
	sysize = (ysize*radius)+0.5;
    } else {
	xrad = atof(argv[3]);
	yrad = atof(argv[4]);
	xrad = xrad/xsize;
	yrad = yrad/ysize;
	sxsize = (xsize*xrad)+0.5;
	sysize = (ysize*yrad)+0.5;
    }
    xfactor = (float)sxsize/xsize;
    yfactor = (float)sysize/ysize;
    if(xfactor>1.0)
	xfactor = 1.0;
    if(yfactor>1.0)
	yfactor = 1.0;
    sprintf(cmd,"izoom %s BLt.rgb %f %f -m",argv[1],xfactor,yfactor);
    system(cmd);
    sprintf(cmd,"izoom BLt.rgb %s %f %f -m -w 1.5",argv[2],1.0/xfactor,1.0/yfactor);
    system(cmd);
    system("rm BLt.rgb");
    exit(0);
}
