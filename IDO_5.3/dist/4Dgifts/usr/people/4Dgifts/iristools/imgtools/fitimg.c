/*
 *	fitimg - 
 *		Uniformly scale a picture to a specific size.
 *
 *				Paul Haeberli - 1988
 */
#include "image.h"
#include "math.h"

int margin = 6;
int frame = 1;

main(argc,argv)
int argc;
char **argv;
{
    int xsize, ysize, zsize;
    int xdest, ydest;
    float iscale;
    int xorig, yorig;
    IMAGE *iimage;
    char cmd[256];

    if( argc<5 ) {
	fprintf(stderr,"usage: fitimg inimage outimage xsize ysize\n");
	exit(1);
    } 
    xdest = atoi(argv[3]);
    ydest = atoi(argv[4]);

    if( (iimage=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"fitimg: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    iclose(iimage);

    if((float)xsize/ysize > (float)(xdest-2*margin)/(ydest-2*margin)) {
        iscale = (float)(xdest-2*margin)/xsize;
        xorig = 0;
        yorig = ((ydest-2*margin)-(iscale*ysize)+0.5)/2.0;
    } else {
        iscale = (float)(ydest-2*margin)/ysize;
        xorig = ((xdest-2*margin)-(iscale*xsize)+0.5)/2.0;
        yorig = 0;
    }
    sprintf(cmd,"nullimg FIt.rgb %d %d %d",xdest,ydest,zsize);
    system(cmd);
    system("invert FIt.rgb FIx.rgb");
    sprintf(cmd,"izoom %s FIs.rgb %f %f -b",argv[1],iscale,iscale);
    system(cmd);
    if(margin>0) {
        sprintf(cmd,"addframe FIs.rgb FIt.rgb %d",frame);
        system(cmd);
	if(margin>1) {
	    sprintf(cmd,"addframe FIt.rgb FIs.rgb %d 255 255 255",margin);
	    system(cmd);
	    xorig--;
	    yorig--;
	} else
	    system("mv FIt.rgb FIs.rgb");
    } 
    sprintf(cmd,"over FIx.rgb FIs.rgb %s %d %d",argv[2],xorig,yorig);
    system(cmd);
    system("rm FIt.rgb FIx.rgb FIs.rgb");
    exit(0);
}
