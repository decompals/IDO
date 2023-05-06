/*
 *	hist - 
 *		Determine the histogram of an image file.
 *
 *				Paul Haeberli - 1984
 *
 */
#include "port.h"
#include "image.h"
#include "gl.h"
#include "device.h"
#include "hist.h"

unsigned short rowbuf[8192];
long xsize, ysize;
long xorg, yorg;
main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *image;
    register unsigned int i, index;
    short val;
    int x, y, z;
    int pixdone, totpixels;
    histogram *hists[3];
    int nhists;
    i=0;		/* just in case rld leaves the stack muddy */
    index=0;		/* just in case rld leaves the stack muddy */

    if( argc<2 ) {
	fprintf(stderr,"usage: hist inimage\n");
	exit(0);
    } 
    if( (image=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"hist: can't open input file %s\n",argv[1]);
	exit(0);
    }
    if(image->max>255) {
	fprintf(stderr,"hist: max can't exceed 255\n");
	exit(0);
    }
    if(image->zsize<3) {
	keepaspect(3,2);
	hists[0] = newhist(image->min,image->max,256);
	nhists = 1;
    } else {
	keepaspect(3,6);
	for(i=0; i<3; i++)
	    hists[i] = newhist(image->min,image->max,256);
	nhists = 3;
    }
    winopen("hist");
    settitle(argv[1],image->min,image->max);
    makeframe();
    totpixels = image->zsize*image->ysize*image->xsize;
    pixdone =0;
    showhists(hists,nhists);
    for(y=0; y<image->ysize; y++) {
	if(image->zsize<3) {
	    getrow(image,rowbuf,y,0);
	    addtohist(hists[0],rowbuf,image->xsize);
	} else {
	    getrow(image,rowbuf,y,0);
	    addtohist(hists[0],rowbuf,image->xsize);
	    getrow(image,rowbuf,y,1);
	    addtohist(hists[1],rowbuf,image->xsize);
	    getrow(image,rowbuf,y,2);
	    addtohist(hists[2],rowbuf,image->xsize);
	}
	percentdone((100.0*pixdone)/totpixels);
	pixdone += image->xsize;
	checkredraw();
	if((y%100) == 0)
	    showhists(hists,nhists);
    }
    percentdone(100.0);
    showhists(hists,nhists);
    qdevice(LEFTMOUSE);
    while(1) {
	switch(qread(&val)) {
	    case REDRAW:
		makeframe();
		showhists(hists,nhists);
		break;
	    case LEFTMOUSE:
		if(val) {
		    x = getvaluator(MOUSEX)-xorg;
		    printf("pixel value is %d\n",(x*256)/xsize);
		}
		break;
	}
    }
}

checkredraw()
{
    short val;

    while(qtest()) {
	if(qread(&val) == REDRAW)
	    makeframe();
    }
}

makeframe()
{
    getorigin(&xorg,&yorg);
    getsize(&xsize,&ysize);
    reshapeviewport();
}

settitle(name,min,max)
char *name;
int min, max;
{
    char oneline[256];

    sprintf(oneline,"hist %s min: %d max: %d\n",name,min,max);
    wintitle(oneline);
}

showhists(hists,n)
histogram *hists[];
int n;
{
    int i;
    float base, delta;

    delta = 1.0/n;
    base = 0.0;
    for(i=0; i<n; i++) {
	pushviewport();
	subport(0.0,1.0,base,base+delta);
	showhist(hists[i]);
	popviewport();
	base += delta;
    }
}
