/*
 *	scrsave - 
 *		Save a part of the screen in an image file.
 *
 *			Paul Haeberli - 1988
 */
#include <stdio.h>
#include <gl/gl.h>
#include "port.h"
#include "image.h"

int xmaxscreen;
int ymaxscreen;

unsigned char *chkmalloc(n)
int n;
{
    unsigned char *lptr;

    lptr = (unsigned char *)malloc(n);
    if(!lptr) {
	fprintf(stderr,"scrsave: malloc of %d bytes failed\n",n);
	exit(1);
    }
    return lptr;
}

int ilimit(min,val,max)
int min,val,max;
{
    if(val<=min)
	return min;
    if(val>=max)
	return max;
    return val;
}

main(argc,argv)
int argc;
char **argv;
{
    long i, y, gotfirst;
    long x1, x2, y1, y2;
    long rgbmode;


    if(argc!=2 && argc!=6 && argc!=3 && argc!=7) {
	fprintf(stderr,"usage: scrsave outimage [x1 x2 y1 y2] [-b]\n");
	exit(1);
    } 
    xmaxscreen = getgdesc(GD_XPMAX)-1;
    ymaxscreen = getgdesc(GD_YPMAX)-1;
    if(argc>=6) {
	x1 = atoi(argv[2]);
	x2 = atoi(argv[3]);
	y1 = atoi(argv[4]);
	y2 = atoi(argv[5]);
    } else {
	x1 = 0;
	y1 = 0;
	x2 = xmaxscreen;
	y2 = ymaxscreen;
    }
    if(argc==7 || argc==3)
	rgbmode = 0;
    else 
	rgbmode = 1;
    foreground();
    /* ratfool, 7/22/91:  inserting explicit prefposition call to 
     * implement workaround to bug #56310 as explained to me by myoung.
     */
    prefposition(0,xmaxscreen-1,0,ymaxscreen-1);
    noport();
    winopen("scrsave");
    savescreen(argv[1],x1,x2,y1,y2,rgbmode);
    exit(0);
}

savescreen(name,x1,x2,y1,y2,rgbmode)
char *name;
int x1, x2, y1, y2;
int rgbmode;
{
    IMAGE *oimage;
    int xsize, ysize;
    int xorg, yorg;
    int writeerr, y;
    unsigned long *scrbuf, *ss;
    unsigned short *rs, *gs, *bs;

/* check the coords passed in */
    x1 = ilimit(0,x1,xmaxscreen);
    x2 = ilimit(0,x2,xmaxscreen);
    y1 = ilimit(0,y1,ymaxscreen);
    y2 = ilimit(0,y2,ymaxscreen);
    xorg = MIN(x1,x2);
    yorg = MIN(y1,y2);
    xsize = ABS(x2-x1)+1;
    ysize = ABS(y2-y1)+1;

/* open the image file */
    if(rgbmode)
	oimage = iopen(name,"w",RLE(1),3,xsize,ysize,3);
    else
	oimage = iopen(name,"w",RLE(1),2,xsize,ysize,1);
    writeerr = 0;

/* malloc buffers */
    rs = (unsigned short *)chkmalloc(xsize*sizeof(short));
    gs = (unsigned short *)chkmalloc(xsize*sizeof(short));
    bs = (unsigned short *)chkmalloc(xsize*sizeof(short));
    scrbuf = (unsigned long *)chkmalloc(xsize*ysize*sizeof(long));

/* read the display */
    readdisplay(xorg,yorg,xorg+xsize-1,yorg+ysize-1,scrbuf,RD_FREEZE);

/* write the data to the image file */
    ss = scrbuf;
    for(y=0; y<ysize; y++) {
	cpacktorgb(ss,rs,gs,bs,xsize);
	if(rgbmode) {
	    if(putrow(oimage,rs,y,0)!=xsize) 
		writeerr = 1;
	    if(putrow(oimage,gs,y,1)!=xsize) 
		writeerr = 1;
	    if(putrow(oimage,bs,y,2)!=xsize) 
		writeerr = 1;
	} else {
	    rgbrowtobw(rs,gs,bs,rs,xsize);
	    if(putrow(oimage,rs,y,0)!=xsize) 
		writeerr = 1;
	}
	ss += xsize;
    }

/* free buffers */
    free(rs);
    free(gs);
    free(bs);
    free(scrbuf);

/* close the image file */
    if(iclose(oimage)<0)
	writeerr = 1;

/* exit with a bad status on write error */
    if(writeerr == 1) 
	exit(1);
    else
	exit(0);
}
