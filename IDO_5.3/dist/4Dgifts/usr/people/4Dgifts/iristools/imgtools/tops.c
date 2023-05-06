/*
 *	This knows how to print images with either 1, 2, 4, or 8 bits per 
 *	pixel, and how to generate different screen densities and screen 
 *	angles.  Postscript data is written to standard out, so use a 
 *	command like:
 *
 *			tops blat.rgb | lp
 *
 *					to actually print a picture.
 *
 *	NOTE: Bugs in the LazerWriter prevent some images from printing.
 *	I know an image 2260 by 27 pixels will not print.
 *
 *
 *				Paul Haeberli - 1988
 *
 */
#include "image.h"
#include "izoom.h"
#include "math.h"

#ifdef AGFA150
#define HYPVAL  1.329513	/* for AGFA 150 line screen onto paper */
#define POWVAL	0.475645
#else
#define HYPVAL  1.0	
#define POWVAL	1.0
#endif

#define DEFLIN	40.0		/* default line screen */

short cortab[256];

#define COLOR_BW	1
#define COLOR_RGB 	2
#define COLOR_CMYK	3

typedef struct printspec {
    float screendensity;
    float screenangle; 
    float xpixperinch; 
    float ypixperinch; 
    float scaletrim; 
    float maxxsize, maxysize;
    float xorg, yorg;
    int bitsper, linescreen;
    int doeps;
    int docolor;
    int nfunc;
    int include;
    char *name;
} printspec;

IMAGE *inimage;

psbwgetrow(buf,y)
short *buf;
int y;
{
    getbwrow(inimage,buf,y);
}

pscolorgetrow(buf,y,z)
short *buf;
int y, z;
{
    getrow(inimage,buf,y,z%inimage->zsize);
}

main(argc,argv)
int argc;
char **argv;
{
    printspec ps;
    IMAGE *image;
    int i;

    if(argc<2) {
	fprintf(stderr,"usage: tops inimage [-l screendensity]\n");
	fprintf(stderr,"                    [-p pixelsperinch]\n");
	fprintf(stderr,"                    [-a screenangle]\n");
	fprintf(stderr,"                    [-b bitsperpixel]\n");
	fprintf(stderr,"                    [-B ]\n");
	fprintf(stderr,"                    [-x xpixelsperinch]\n");
	fprintf(stderr,"                    [-y ypixelsperinch]\n");
	fprintf(stderr,"                    [-t scaletrim\n");
	fprintf(stderr,"                    [-m maxxinches maxyinches]\n");
	fprintf(stderr,"                    [-h horrizontal line screen]\n");
	fprintf(stderr,"                    [-o xorg yorg]\n");
	fprintf(stderr,"                    [-rgb]\n");
	fprintf(stderr,"                    [-RGB]\n");
	fprintf(stderr,"                    [-cmyk]\n");
	fprintf(stderr,"                    [-CMYK]\n");
	fprintf(stderr,"                    [-eps ]\n");
	fprintf(stderr,"                    [-I ]\n");
	exit(1);
    }
    image = iopen(argv[1],"r");
    if(!image) {
	fprintf(stderr,"tops: can't open input image file\n");
	exit(1);
    }
    ps.screendensity = DEFLIN;
    ps.screenangle = 45.0;
    ps.xpixperinch = -1.0;
    ps.ypixperinch = -1.0;
    ps.scaletrim = 1.0;
    ps.maxxsize = 576.0;
    ps.maxysize = 756.0;
    ps.bitsper = 8;
    ps.linescreen = 0;
    ps.xorg = 18.0;
    ps.yorg = 18.0;
    ps.docolor = COLOR_BW;
    ps.nfunc = 1;
    ps.name = argv[1];
    ps.include = 0;
    ps.doeps = 0;
    for(i=2; i<argc; i++) {
	if(argv[i][0] == '-') {
	    switch(argv[i][1]) {
		case 'b':
		    i++;
		    ps.bitsper = atoi(argv[i]);
		    switch(ps.bitsper) {
			case 1:
			case 2:
			case 4:
			case 8:
			    break;
		 	default:
			    fprintf(stderr,"tops: bits per pixel must be 1, 2, 4, or 8\n");
			    exit(1);
		    }
		    break;
		case 'l':
		    i++;
		    ps.screendensity = atof(argv[i]);
		    break;
		case 'a':
		    i++;
		    ps.screenangle = atof(argv[i]);
		    break;
		case 'B':
		    usebinaryps(1);
		    break;
		case 'p':
		    i++;
		    ps.ypixperinch = ps.xpixperinch = atof(argv[i]);
		    break;
		case 'x':
		    i++;
		    ps.xpixperinch = atof(argv[i]);
		    break;
		case 'y':
		    i++;
		    ps.ypixperinch = atof(argv[i]);
		    break;
		case 't':
		    i++;
		    ps.scaletrim = atof(argv[i]);
		    break;
		case 'm':
		    i++;
		    ps.maxxsize = 72.0*atof(argv[i]);
		    i++;
		    ps.maxysize = 72.0*atof(argv[i]);
		    break;
		case 'h':
		    ps.linescreen = 1;
		    break;
		case 'o':
		    i++;
		    ps.xorg = 72.0*atof(argv[i]);
		    i++;
		    ps.yorg = 72.0*atof(argv[i]);
		    break;
		case 'r':
		    ps.docolor = COLOR_RGB;
		    ps.nfunc = 1;
		    break;
		case 'R':
		    ps.docolor = COLOR_RGB;
		    ps.nfunc = 3;
		    break;
		case 'c':
		    ps.docolor = COLOR_CMYK;
		    ps.nfunc = 1;
		    break;
		case 'C':
		    ps.docolor = COLOR_CMYK;
		    ps.nfunc = 4;
		    break;
		case 'e':
		    ps.doeps = 1;
		    break;
		case 'I':
		    ps.include = 1;
		    ps.doeps = 1;
		    break;
	    }
	}
    }
    tops(image,&ps);
    exit(0);
}

void definecolorimageop()
{
    printf("/bwproc {\n");
    printf("    rgbproc\n");
    printf("    dup length 3 idiv string 0 3 0\n");
    printf("    5 -1 roll {\n");
    printf("    add 2 1 roll 1 sub dup 0 eq\n");
    printf("    { pop 3 idiv 3 -1 roll dup 4 -1 roll dup\n");
    printf("      3 1 roll 5 -1 roll put 1 add 3 0 }\n");
    printf("    { 2 1 roll } ifelse\n");
    printf("    } forall\n");
    printf("    pop pop pop\n");
    printf("} def\n");
    printf("systemdict /colorimage known not {\n");
    printf("    /colorimage {\n");
    printf("	pop\n");
    printf("	pop\n");
    printf("	/rgbproc exch def\n");
    printf("	{ bwproc } image\n");
    printf("    } def\n");
    printf("} if\n");
}

tops(image,ps)
IMAGE *image;
printspec *ps;
{
    int xsize, ysize;
    int llx, lly, urx, ury;
    float doxscale, doyscale, ppixscale, ppiyscale;
    float aspect;

    inimage = image;
    hypcurve(HYPVAL,POWVAL,cortab);
    xsize = image->xsize;
    ysize = image->ysize;
    ps->xpixperinch /= ps->scaletrim;
    ps->ypixperinch /= ps->scaletrim;
    if(ps->xpixperinch > 0.0) 
	aspect = (ysize/ps->ypixperinch)/(xsize/ps->xpixperinch);
    else
	aspect = ysize/(float)xsize;

    if(aspect < ps->maxysize/ps->maxxsize) {
	doxscale = ps->maxxsize/xsize;
	doyscale = doxscale;
    } else {
	doxscale = ps->maxysize/ysize;
	doyscale = doxscale;
    }

    if(ps->xpixperinch > 0.0) {
	ppixscale = 72.0/ps->xpixperinch;
	ppiyscale = 72.0/ps->ypixperinch;
	doxscale = ppixscale;
	doyscale = ppiyscale;
    }

/* put out the header */
    if(ps->doeps) {
	printf("%%!PS-Adobe-2.0 EPSF-1.2\n");
	llx = ps->xorg+0.499;
	lly = ps->yorg+0.499;
	urx = (llx+doxscale*xsize)+0.499;
	ury = (lly+doyscale*ysize)+0.499;
	doxscale = (float)(urx-llx)/xsize;
	doyscale = (float)(ury-lly)/ysize;

	printf("%%%%Creator: tops on IRIS workstation\n");
	printf("%%%%BoundingBox: %d %d %d %d\n",llx,lly,urx,ury);
	printf("%%%%EndComments\n");
	printpreview(image);
	printf("gsave\n");
	printf("%f %f translate\n",ps->xorg,ps->yorg+doyscale*ysize);
	if(ps->include) {
	    printf("%f %f scale\n",doxscale*xsize,-doyscale*ysize);
	    dopsinclude(image,ps);
	    return;
  	}
    } else {
	printf("%%!\n");
	printf("gsave\n");
	printf("{} settransfer\n");
	if(ps->linescreen) 
	    printf("%f %f {pop} setscreen\n",ps->screendensity,ps->screenangle);
	else
	    printf("%f %f {dup mul exch dup mul add 1 exch sub} setscreen\n",
				       ps->screendensity,ps->screenangle);
	printf("%f %f translate\n",ps->xorg,ps->yorg+ps->maxysize);
    }
    definecolorimageop();
/* do the proper image translation and scaling */
    printf("%f %f scale\n",doxscale*xsize,-doyscale*ysize);
    switch(ps->docolor) {
	case COLOR_BW:
	    tobwps(stdout,psbwgetrow,cortab,ps->bitsper,xsize,ysize);
	    break;
	case COLOR_RGB:
	    torgbps(stdout,pscolorgetrow,cortab,ps->bitsper,xsize,ysize,ps->nfunc);
	    break;
	case COLOR_CMYK:
	    tocmykps(stdout,pscolorgetrow,cortab,ps->bitsper,xsize,ysize,ps->nfunc);
	    break;
    }
    printf("grestore\n");
    if(ps->doeps) 
	printf("\n");
    else
	printf("\nshowpage\n");
}

#define PREV_MAXXSIZE	250
#define PREV_MAXYSIZE	250

zoombwgetrow(buf,y)
short *buf;
int y;
{
    getbwrow(inimage,buf,inimage->ysize-1-y);
}

printpreview(image)
IMAGE *image;
{
    unsigned char cbuf[(PREV_MAXXSIZE/8+1)];
    zoom *zm;
    short *buf;
    int anx, any, bnx, bny;
    int x, y, nbytes;

    anx = image->xsize;
    any = image->ysize;
    if(anx*PREV_MAXYSIZE>any*PREV_MAXXSIZE) {
	bnx = PREV_MAXXSIZE;
	bny = (bnx*any)/anx;
    } else {
	bny = PREV_MAXYSIZE;
	bnx = (bny*anx)/any;
    }
    nbytes = 1+((bnx-1)/8);
    buf = (short *)malloc(bnx*sizeof(short));
    zm = newzoom(zoombwgetrow,anx,any,bnx,bny,IMPULSE,1.0);
    printf("%%%%BeginPreview: %d %d %d %d\n",bnx,bny,1,bny);
    for(y=0; y<bny; y++) {
	getzoomrow(zm,buf,y);
	ditherrow(buf,y,bnx);
	rowtobits(buf,cbuf,bnx);
	printf("%% ");
	for(x=0; x<nbytes; x++) 
	    printf("%02x",cbuf[x]);
	printf("\n");
    }
    printf("%%%%EndPreview\n");
    freezoom(zm);
}

#define INC_MAXXSIZE	50
#define INC_MAXYSIZE	50

zoom *inczm;

includegetrow(buf,y)
short *buf;
int y;
{
    getzoomrow(inczm,buf,y);
}

dopsinclude(image,ps)
IMAGE *image;
printspec *ps;
{
    int anx, any, bnx, bny;
    int x, y;

    anx = image->xsize;
    any = image->ysize;
    if(anx*INC_MAXYSIZE>any*INC_MAXXSIZE) {
	bnx = INC_MAXXSIZE;
	bny = (bnx*any)/anx;
    } else {
	bny = INC_MAXYSIZE;
	bnx = (bny*anx)/any;
    }
    inczm = newzoom(psbwgetrow,anx,any,bnx,bny,IMPULSE,1.0);
    printf("( SGIImageIncludeBegin %s ) pop\n",ps->name);
    tobwps(stdout,includegetrow,cortab,ps->bitsper,bnx,bny);
    printf("( SGIImageIncludeEnd %s ) pop\n",ps->name);
    printf("grestore\n");
    freezoom(inczm);
}
