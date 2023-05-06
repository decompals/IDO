/*
 *	frompic -
 *		convert a MOVIE BYU .PIC image file to IRIS image file format.
 *
 *				Paul Haeberli - 1990
 */
#include "gl.h"
#include "image.h"

#define MAXWIDTH	8196

struct pixel_run {			
    unsigned char r,g,b,count;
} run;

struct VIEWPORT {
    Screencoord l,r,b,t;
} screen;

unsigned char r[MAXWIDTH];
unsigned char g[MAXWIDTH];
unsigned char b[MAXWIDTH];
unsigned short rbuf[MAXWIDTH];
unsigned short gbuf[MAXWIDTH];
unsigned short bbuf[MAXWIDTH];

main(argc,argv)
int argc;
char *argv[];
{
    int xsize, ysize;
    int y;
    FILE *fp;
    int end_of_run, count, i;
    IMAGE *oimage;

    if (argc < 2)	{
	printf("usage: frompic input.PIC out.rgb\n");
	exit(1);
    }
    fp = fopen(argv[1],"r");
    if(!fp) {
	printf("frompic: con't open input file %s\n",argv[1]);
	exit(1);
    }
    fread(&screen,sizeof(screen),1,fp);
    xsize = screen.r - screen.l +1;
    ysize = screen.t - screen.b +1;
    oimage = iopen(argv[2],"w",RLE(1),3,xsize,ysize,3);
    for(y=0; y<ysize; y++) {
	count = 0;
	while( count < xsize ) {
	    fread(&run,sizeof(run),1,fp );
	    if( run.count == 0 )
		end_of_run = xsize;
	    else 
		end_of_run = count + run.count;
	    for(i=count; i<end_of_run; i++) {
		r[i] = run.r;
		g[i] = run.g;
		b[i] = run.b;
	    }
	    count = end_of_run;
	}
	ctos(r,rbuf,xsize);
	ctos(g,gbuf,xsize);
	ctos(b,bbuf,xsize);
	putrow(oimage,rbuf,ysize-1-y,0);
	putrow(oimage,gbuf,ysize-1-y,1);
	putrow(oimage,bbuf,ysize-1-y,2);
    }
    iclose(oimage);
    fclose(fp);
    exit(0);
}
