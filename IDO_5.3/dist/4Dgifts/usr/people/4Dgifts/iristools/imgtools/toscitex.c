/*
 *	toscitex -
 *		Convert an Iris image into a Scitex CT2T image. This can
 *	be sent to Solzer+Hail in San Francisco, or Kede/Orent in Sunnyvale. 
 *
 *				Paul Haeberli - 1988
 */
#include "math.h"
#include "image.h"
#include "gl.h"
#include "lum.h"

/* config stuff */

#ifdef TOSTAR
#define HYPVAL			(0.600)
#define POWVAL			(1.000)
#define XPIXELSPERINCH		(304.8)
#define YPIXELSPERINCH		(304.8)
#define SCREENLINESPERINCH 	(175)
#define FLIPCODE		2
#else
#define HYPVAL			(0.684)
#define POWVAL			(0.639)
#define XPIXELSPERINCH		(300.0)
#define YPIXELSPERMM		(12.0)
#define SCREENLINESPERINCH 	(175)
#define FLIPCODE		3
#endif

#define DEFBORDER	(60)    /* width of white border around image */

/* defines follow */

#define DEFTAPEDEV	"/dev/rmt/xmt0d0nr.6250"
#define MAXWIDTH	(3*4096)

#define PUT_CMYK		0
#define PUT_CMY			1
#define PUT_K			2

unsigned char cbuf[4*MAXWIDTH];
short rbuf[MAXWIDTH];
short gbuf[MAXWIDTH];
short bbuf[MAXWIDTH];
short cortab[256];
int outmode = PUT_CMYK;
int regmarkmode = 0;
int linearmode = 0;
int nolerpmode = 0;
int border = DEFBORDER;
char *tapefile;
int tofile;
char *inimages[100];

printusage(name)
char *name;
{
    fprintf(stderr,"usage: %s [-m] [-n] [-l] [-o tapedev] [-p] imagefiles . . . \n",name);
}

main(argc,argv)
int argc;
char **argv;
{
    int i, nimages;
    FILE *logf;

    tapefile = DEFTAPEDEV;
    tofile = 0;
    if(argc<2) {
	printusage(argv[0]);
	exit(1);
    }
    nimages=0;
    for(i=1; i<argc; i++) {
	if(argv[i][0] == '-') {
	    switch(argv[i][1]) {
		case 'm': 
		    regmarkmode++;
		    border = 0;
		    break;
		case 'n': 
		    nolerpmode++;
		    break;
		case 'l': 
		    linearmode++;
		    break;
		case 'o':
		    i++;
		    tapefile = argv[i];
		    if(strncmp(tapefile,"/dev",4) != 0)
		       tofile = 1;
		    break;
		case 'p': 
		    border = 0;
		    fprintf(stderr,"toscitex: No border added to image\n");
		    break;
	    }
	} else 
	    inimages[nimages++] = argv[i];
    }
    makecortab(linearmode);
    if(nimages<1) {
	printusage(argv[0]);
	exit(1);
    }
    if(tofile && nimages>1) {
	fprintf(stderr,"toscitex: only one image allowed when writing to file\n");
	exit(1);
    }
    rewindtape();
    logf = fopen("tapelog","w");
    if(!logf) {
	fprintf(stderr,"toscitex: can't open log file for writing\n");
	exit(1);
    }
    if(nimages == 1)
	fprintf(logf,"This tape contains 1 image in CT2T format.\n");
    else
	fprintf(logf,"This tape contains %d images in CT2T format.\n",nimages);
#ifdef TOSTAR
    fprintf(logf,"It should be plotted to film at Star Graphics 415-345-3321.\n");
#else
    fprintf(logf,"It should be plotted to film at Kedie/Orent 408-734-9005.\n");
#endif
    fprintf(logf,"Magic hypval is %f, while powval is %f.\n",HYPVAL,POWVAL);
    fprintf(logf,"Each image is written with four bytes per pixel.\n");
    fprintf(logf,"CMYK values are interleaved across each scan line.\n\n");
    switch(outmode) {
	case PUT_CMYK:
	    fprintf(logf,"CMYK images.\n");
	    break;
	case PUT_CMY:
	    fprintf(logf,"WARNING: CMY only images.\n");
	    break;
	case PUT_K:
	    fprintf(logf,"WARNING: K only images.\n");
	    break;
    }
    if(regmarkmode)
	fprintf(logf,"WARNING: IMAGES WRITTEN WITH REGMARK MODE.\n");
    if(linearmode)
	fprintf(logf,"WARNING: IMAGES WRITTEN WITH LINEAR LOOKUP.\n");
    if(nolerpmode) {
	fprintf(logf,"WARNING: IMAGES WRITTEN WITH NO LERP.\n");
	cmyksetup(160,160,0,255,0);
    } else {
	cmyksetup(160,160,0,255,4);
    }
    fprintf(logf,"\n");
    for(i=0; i<nimages; i++)
	writeimage(logf,inimages[i]);
    fprintf(logf,"\nend of tape - no errors!!\n");
    fclose(logf);
    rewindtape();
    exit(0);
}

writeimage(logf,name)
FILE *logf;
char *name;
{
    int y, ixsize, iysize, izsize;
    int txsize, tysize;
    int rowbytes;
    int nbytes, outf, nblocks;
    IMAGE *image;

    image = iopen(name,"r");
    if(!image) {
	fprintf(stderr,"toscitex: can't open input image %s\n",name);
	exit(1);
    }
    ixsize = image->xsize;
    iysize = image->ysize;
    izsize = image->zsize;
    txsize = ixsize+2*border;
    tysize = iysize+2*border;
    rowbytes = 4*txsize;
    nblocks = tysize;
    fprintf(logf,"imagename: %s\txsize: %d\tysize: %d \tnum tape blocks %d.\n",name,txsize,tysize,nblocks);
    fprintf(logf,"Each tape block contains %d bytes.\n",4*txsize);
#ifdef TOSTAR
    fprintf(logf,"X height:  %3.3f inches\n",txsize/XPIXELSPERINCH);
    fprintf(logf,"Y width: %3.3f inches\n\n",tysize/YPIXELSPERINCH);
#else
    fprintf(logf,"X height:  %3.3f inches\n",txsize/XPIXELSPERINCH);
    fprintf(logf,"Y width: %4.2f millimeters\n\n",tysize/YPIXELSPERMM);
#endif
    fprintf(logf,"line screen: %d lines per inch.\n\n",SCREENLINESPERINCH);
    if(tofile) 
	outf = creat(tapefile,0666);
    else
	outf = open(tapefile,1);
    if(outf<0) {
	fprintf(stderr,"toscitex: can't open [%s] for writing\n",tapefile);
	exit(1);
    }
    bzero(cbuf,rowbytes);
    for(y=0; y<border; y++) {
	nbytes = write(outf,cbuf,rowbytes);
	if(nbytes != rowbytes) {
	    fprintf(stderr,"toscitex: bad byte count on write %d\n",nbytes);
	    exit(1);
	}
    }
    tpercentdone(0.0);
    for(y=0; y<iysize; y++) {
	if(izsize<3) {
	    getfliprow(image,rbuf,y,0,FLIPCODE);
	    getfliprow(image,gbuf,y,0,FLIPCODE);
	    getfliprow(image,bbuf,y,0,FLIPCODE);
	} else {
	    getfliprow(image,rbuf,y,0,FLIPCODE);
	    getfliprow(image,gbuf,y,1,FLIPCODE);
	    getfliprow(image,bbuf,y,2,FLIPCODE);
	}
	cvt(rbuf,gbuf,bbuf,cbuf+4*border,ixsize);
	nbytes = write(outf,cbuf,rowbytes);
	if(nbytes != rowbytes) {
	    fprintf(stderr,"toscitex: bad byte count on write %d\n",nbytes);
	    exit(1);
	}
	tpercentdone(100.0*y/(iysize-1.0));
    }
    tpercentdone(100.0);
    bzero(cbuf,rowbytes);
    for(y=0; y<border; y++) {
	nbytes = write(outf,cbuf,rowbytes);
	if(nbytes != rowbytes) {
	    fprintf(stderr,"toscitex: bad byte count on write %d\n",nbytes);
	    exit(1);
	}
    }
    close(outf);
    iclose(image);
}

cvt(rbuf,gbuf,bbuf,cbuf,n)
register short *rbuf, *gbuf, *bbuf;
register unsigned char *cbuf;
register int n;
{
    int c, m, y, k;
    int r, g, b;

    while(n--) {
	r = *rbuf++;
	g = *gbuf++;
	b = *bbuf++;
	if(regmarkmode) {
	   if(r<255)
	       r--;
	   if(g<255)
	       g--;
	   if(b<255)
	       b--;
	}
	if(r<0 || g<0 || b<0) {
	    if(r>=0 || r>=0 || b>=0) {
		fprintf(stderr,"toscitex: range error in regmark mode\n");
		exit(1);
	    }
	    c = 255;
	    m = 255;
	    y = 255;
	    k = 255;
	} else {
	    switch(outmode) {
		case PUT_CMYK:
		    rgb_to_cmyk(r,g,b,&c,&m,&y,&k,cortab);
		    break;
		case PUT_CMY:
		    rgb_to_cmyonly(r,g,b,&c,&m,&y,&k,cortab);
		    break;
		case PUT_K:
		    rgb_to_konly(r,g,b,&c,&m,&y,&k,cortab);
		    break;
	    }
	}
	*cbuf++ = c;
	*cbuf++ = m;
	*cbuf++ = y;
	*cbuf++ = k;
    }
}

makecortab(lintab)
int lintab;
{
    int i;

    if(lintab) {
	for(i=0; i<256; i++) 
	    cortab[i] = i;
    } else {
	hypcurve(HYPVAL,POWVAL);
    }
}

double hypfunc(p,r,x)
double p, r, x;
{
    double b, y;

    b = (1.0/p) - p;
    y = x;
    y = 1.0-(1.0/((y*b)+p) - p)/b;
    y = pow(y,r);
    return y;
}

hypcurve(hypval,powval)
float hypval, powval;
{
    double x, y;
    int i;

    for(i=0; i<256; i++) {
	x = i/255.0;
	y = hypfunc(hypval,powval,x);
	cortab[i] = (255*y)+0.5;
    }
}

rewindtape()
{
    char cmd[256];

    if(!tofile) {
	sprintf(cmd,"mt -t %s rewind",tapefile);
	system(cmd);
    }
}
