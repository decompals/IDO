/*
 *	bgpaste - paste an image or list of images into the root window.
 *
 *			dave ratcliffe and Paul Haeberli - 1991	
 */
#include <gl/gl.h>
#include <gl/device.h>
#include <image.h>
#include "port.h"
#include "dispimg.h"
#include "rct.h"

#define MAXIMGWIDTH (4096)
#define RGB 1
#define ORG 2

DISPIMAGE *di;
DISPIMAGE *readit();

int drawit();

typedef struct {
    rct imgrct;   /* stores x/y min max values */
    long xsize;   /* width of original image   */
    long ysize;   /* height of original image  */
    char *file;   /* filename of image         */
} MULTIMAGE;

MULTIMAGE imgs[20];

rct imgrct, allrct, scrnrct, visrct, tmprct;        /* rectangle structures */

unsigned long *imgbuf;        /* buffer containing composite image using -n */

unsigned short rbuf[MAXIMGWIDTH];     /* buffers used to read rgb data */
unsigned short gbuf[MAXIMGWIDTH];     /* from inimages and then write  */
unsigned short bbuf[MAXIMGWIDTH];     /* them to lrectwrite buffer     */

long numimgs;                         /* "-n" flag's value          */
long xorig, yorig;                    /* origin of "current" image  */
long xsize, ysize;                    /* size of "current" image    */
long xscreensize, yscreensize;        /* resolution of current scrn */

long resetorg = FALSE;                /* set to TRUE with -o flag */
long multiple_imgs = FALSE;           /* set to TRUE when -n > 1  */

long xless = 0;                       /* true if image->xsize < xscreensize */
long yless = 0;                       /* true if image->ysize < yscreensize */
long red = 122;                       /* default setting */
long gre = 122;                       /* for background  */
long blu = 122;                       /* rgb triplet     */


main(argc,argv)
int argc;
char **argv;
{
    short val;
    long fileindex;


    if( argc<2 ) {
	fprintf(stderr,"usage: bgpaste [-t r g b] [-o xorig yorig] inimage\n");
	fprintf(stderr,"usage: bgpaste [-t r g b] -n numimgs xorg yorg img [xorg yorg img . . . ]\n");
	exit(1);
    } 

    xscreensize = getgdesc(GD_XPMAX);
    yscreensize = getgdesc(GD_YPMAX);
    
    getargs(argc, argv, &fileindex);

    if (multiple_imgs)
        readem(argv);
    else
        di = readit(argv[fileindex]);

    while (1) {
        switch(qread(&val)) {
            case REDRAW:
                qreset();
                drawit();
                break;
        }
    }
}


/*
 * readit is used if a single image is specified.  
 *    (1st instance of bgpaste usage indicated at the beginning of main)
 */
DISPIMAGE *readit(filename)
char *filename;
{
    register IMAGE *image;
    register DISPIMAGE *di;
    long x1, x2, y1, y2;
    long xdisplen, ydisplen;
    long xorg, yorg;

/* allocate the display image struct, and open the input file for reading */
    di = (DISPIMAGE *)malloc(sizeof(DISPIMAGE));
    if( (image=iopen(filename,"r")) == NULL ) {
	fprintf(stderr,"paste: can't open input file %s\n",filename);
	exit(1);
    }

/* calculate the image size */
    xsize = image->xsize;
    ysize = image->ysize;
    if (resetorg) {
	xorg = xorig;
	if (xorig < 0) {
            xdisplen = xsize + xorig;
	    x1 = abs(xorig);
	    xorg = 0;
	    if (xdisplen > xscreensize)
	        x2 = xscreensize -1 + x1;
	    else {
	        x2 = xsize - 1;
    		xless = 0x1;
	    }
	    xsize = xdisplen;
	} else {
	    xdisplen = xsize;
	    x1 = 0;
	    if (xdisplen > (xscreensize - xorig))
	        xdisplen = xscreensize - xorig;
	    if (xdisplen < xscreensize)
    		xless = 0x2;
	    x2 = xdisplen - 1;
	    xsize = xdisplen;
	}
	yorg = yorig;
	if (yorig < 0) {
            ydisplen = ysize + yorig;
	    y1 = abs(yorig);
	    yorg = 0;
	    if (ydisplen > yscreensize)
		y2 = yscreensize + y1;
	    else {
	        y2 = ysize - 1;
		yless = 0x1;
	    }
	    ysize = ydisplen;
	} else {
	    ydisplen = ysize;
	    y1 = 0;
	    if (ydisplen > (yscreensize - yorig)) 
	        ydisplen = yscreensize - yorig;
            if (ydisplen < yscreensize)
    		yless = 0x2;
	    y2 = ydisplen - 1;
	    ysize = ydisplen;
	}
    } else {
	if (xsize > xscreensize) {
	    x1 = (xsize - xscreensize) / 2;
	    x2 = x1 + xscreensize - 1;
	    xorg = 0;
	} else {
	    x1 = 0;
	    x2 = xsize - 1;
	    xorg = (xscreensize -1 - xsize) / 2;
	    if (xsize < xscreensize)
		xless = 0x3;
	}

	if (ysize > yscreensize) {
	    y1 = (ysize - yscreensize - 1) / 2;
	    y2 = y1 + yscreensize - 1;
	    yorg = 0;
	} else  {
	    y1 = 0;
	    y2 = ysize - 1;
	    yorg = (yscreensize - 1 - ysize) / 2;
	    if (ysize < yscreensize)
		yless = 0x3;
	}
    }

/* open the window */
    imakebackground();
    winopen("bgpaste");
    subpixel(1);
    wintitle(filename);

/* set the display mode for the image */
    setimagemode(image);

/* fill in the background */
    rgbi(red,gre,blu);
    clear();

/* define the projection */
    makeframe();

/* make the display region:
 *    x1, y1, x2, y2   the region inside the image itself (img coord space)
 *    1                flag (TRUE) shows image as we make this display region 
 *    xorg, yorg       display origin of img indicated by {x1,y1,x2,y2} 
 *                     relative to the current viewport (root window).
 */
    di = makedisprgn(image, x1, x2, y1, y2, 1, xorg, yorg);
    iclose(image);

/* all done at this point, but first we need to assign the x/y orig/size
 * for use in drawit()'s fillrecti and drawimage calls below.
 */
    xorig = xorg;
    yorig = yorg;
    xsize = xorig+di->xsize;
    ysize = yorig+di->ysize;

    return di;
}


clearbuf(unsigned long *buf, long npix, long r, long g, long b)
{
    unsigned long c;

    c = (r<<0)+(g<<8)+(b<<16);
    while(npix--)
        *buf++ = c;
}


/*
 * readem is used if a list of 1 or more images is specified.  
 *    (2nd instance of bgpaste usage indicated at the beginning of main)
 */
readem()
{
    long y, i;
    IMAGE *iimage;
    unsigned long *imgbufptr; 
    unsigned long *imgbufrow;
    long xleft, ybot, ytop;
    long xdisplen, ydisplen;
    long xorg, yorg;
    long visxsize, visysize;
    long dx, dy;


    /* go thru the list of images listed and calculate 
     * the union of their min and max x/y values 
     */
    for (i=0; i<numimgs; i++) {
	rctset(&imgrct, imgs[i].imgrct.xmin, imgs[i].imgrct.ymin,
	                imgs[i].imgrct.xmax, imgs[i].imgrct.ymax);
	if (i==0)
	    allrct = imgrct;
	else
	    rctunion(&imgrct, &allrct, &allrct);
    }

    /* now define the rectangle structure for the screen size */
    rctset(&scrnrct,0,0,xscreensize-1,yscreensize-1);

    /* now calculate the visual intersection of allrct w/scrnrct */
    if (rctinter(&allrct,&scrnrct,&visrct) == 0) {
	fprintf(stderr,"none of the images you specified are ");
	fprintf(stderr,"visible.  pleeze recheck yer origins\n");
	exit(0);
    }

    /* calc the size of the visual rectangle which will be imgbuf's size */
    visxsize = visrct.xmax - visrct.xmin + 1;
    visysize = visrct.ymax - visrct.ymin + 1;

    imgbufrow = (unsigned long *) malloc(MAXIMGWIDTH*sizeof(long));
    imgbuf    = (unsigned long *) malloc(visxsize*visysize*sizeof(long));
    clearbuf(imgbuf,visxsize*visysize,red,gre,blu);

/* now begin to "plant" the images in the order they are enumerated */
    for (i=0; i<numimgs; i++) {

	/* set image # i as the currect "rectangle" */
	rctset(&imgrct, imgs[i].imgrct.xmin, imgs[i].imgrct.ymin,
	                imgs[i].imgrct.xmax, imgs[i].imgrct.ymax);

	/* confirm some portion of image # i is visible */
     	if (rctinter(&imgrct, &visrct, &tmprct) == 1) {

            if( (iimage=iopen(imgs[i].file,"r")) == NULL ) {
                fprintf(stderr,"imgs: can't open input file ");
                fprintf(stderr,"%s\n",imgs[i].file);
                exit(1);
            }

/* determine what part of the image # i to transfer into our composite */
            xorg = imgs[i].imgrct.xmin;
            if (xorg < 0) {
                xdisplen = imgs[i].xsize + xorg;
                xleft = abs(xorg);
                if (xdisplen > xscreensize)
                    xdisplen = xleft + xscreensize;
                else {
                    xless = 0x1;
                }
            } else {
                xdisplen = imgs[i].xsize;
                xleft = 0;
                if (xdisplen > (xscreensize - xorg))
                    xdisplen = xscreensize - xorg;
                if (xdisplen < xscreensize)
                    xless = 0x2;
            }

            yorg = imgs[i].imgrct.ymin;
            if (yorg < 0) {
                ydisplen = imgs[i].ysize + yorg;
                ybot = abs(yorg);
                if (ydisplen > yscreensize)
                    ytop = ydisplen = (yscreensize + ybot) - 1;
                else {
                    ytop = imgs[i].ysize - 1;
                    yless = 0x1;
                }
            } else {
                ydisplen = imgs[i].ysize;
                ybot = 0;
                if (ydisplen > (yscreensize - yorg)) {
                    ydisplen = yscreensize - yorg;
                    ytop = ydisplen - 1;
                } else {
                    yless = 0x2;
                    ytop = imgs[i].ysize - 1;
		}
            }

/* now transfer image # i into the buffer to be displayed */
    /* calc the x and y deltas to be used as an offset into imgbuf */
	    dx = MAX((imgs[i].imgrct.xmin - visrct.xmin),0);
	    dy = MAX((imgs[i].imgrct.ymin - visrct.ymin),0);
    /* update imbbufptr to point to the current row in imgbuf to be painted */
            imgbufptr = &imgbuf[(dy*visxsize)+dx];

	    for (y=ybot; y<=ytop; y++) {

		getrow(iimage,rbuf,y,0);   /* get row # y from image # i */
		getrow(iimage,gbuf,y,1);
		getrow(iimage,bbuf,y,2);

                /* convert this rgb row into cpack (long) format */
		rgbtocpack(rbuf,gbuf,bbuf,imgbufrow,iimage->xsize);

		/* now copy this row, w/the proper x offset into imgbuf */
		bcopy(imgbufrow+xleft,imgbufptr,(xdisplen)*sizeof(long));

		/* add one length of imgbuf's "xsize" to imgbufptr*/
		imgbufptr += visxsize;
	    }

	    iclose(iimage);

	} else {
	    fprintf(stderr,"%s falls entirely outside the visible screen\n",
		      imgs[i].file);
	    fprintf(stderr,"based on the origin [%d,%d] you gave\n",
		      imgs[i].imgrct.xmin,imgs[i].imgrct.ymin);
        }

    }  /* end   "for (i=0; i<numimgs; i++)" */

/* open the window */
    imakebackground();
    winopen("bgpaste");
    RGBmode();
    gconfig();
    subpixel(1);

/* all done at this point, but first we need to assign the x/y orig/size
 * for use in drawit()'s fillrecti and lrectwrite calls below.
 */
    xorig=visrct.xmin;
    yorig=visrct.ymin;
    xsize=xorig+visxsize;
    ysize=yorig+visysize;

    drawit();
}


drawit()
{
    long x1, x2, y1, y2;
    
    makeframe();
    if (xless || yless) {
	rgbi(red, gre, blu);
	if (xless && yless) {
	    fillrecti(0,0,xscreensize,yorig);            /* bottom rectangle */
	    fillrecti(0,ysize,xscreensize,yscreensize);  /* top rectangle    */
	    fillrecti(0,yorig,xorig,ysize);              /* left rectangle   */
	    fillrecti(xsize,yorig,xscreensize,ysize);    /* right rectangle  */
	} else if (xless) {
	    fillrecti(0,yorig,xorig,ysize);              /* left rectangle   */
	    fillrecti(xsize,yorig,xscreensize,ysize);    /* right rectangle  */
	} else {
	    fillrecti(0,0,xscreensize,yorig);            /* bottom rectangle */
	    fillrecti(0,ysize,xscreensize,yscreensize);  /* top rectangle    */
	}
    }
    if (multiple_imgs)
        lrectwrite(xorig, yorig, xsize-1, ysize-1, imgbuf);
    else
        drawimage(di,xorig,yorig);
}


makeframe()
{
    viewport(0,xscreensize-1,0,yscreensize-1);
    ortho2(-0.5,(float)(xscreensize-0.5),-0.5,(float)(yscreensize-0.5));
}


getargs(argc, argv, fileindex)
int argc;
char **argv;
long *fileindex;
{
    long i, fi, ni;
    long flag = FALSE;
    long dash = FALSE;
    long file = FALSE;


    for(i=1; i<argc; i++) {
	if (argv[i][0] == '-') {
	    dash = TRUE;
	    if(strcmp(argv[i],"-t") == 0) {
		if ((!flag && (argc<6)) || (flag && (argc != 9)))
		    printerrorexit();
		else {
		    red = chkgetarg(argv[++i],RGB);
		    gre = chkgetarg(argv[++i],RGB);
		    blu = chkgetarg(argv[++i],RGB);
		}
		flag = TRUE;
	    }
	    if(strcmp(argv[i],"-n") == 0) {
		multiple_imgs = TRUE;
                numimgs = atoi(argv[++i]);
                for (fi=0; fi<numimgs; fi++) {
                    imgs[fi].imgrct.xmin = chkgetarg(argv[++i],ORG);
                    imgs[fi].imgrct.ymin = chkgetarg(argv[++i],ORG);
                    if (!isfile(argv[++i])) {
                        fprintf(stderr,"%s:  arg # %d, %s",argv[0],i,argv[i]);
			fprintf(stderr,", is not an image file.\n");
                        exit(0);
                    }
                    imgs[fi].file = argv[i];
                    sizeofimage(imgs[fi].file,&imgs[fi].xsize,&imgs[fi].ysize);
                    imgs[fi].imgrct.xmax=imgs[fi].imgrct.xmin+imgs[fi].xsize-1;
                    imgs[fi].imgrct.ymax=imgs[fi].imgrct.ymin+imgs[fi].ysize-1;
                }
		flag = TRUE;
            }
	    if(strcmp(argv[i],"-o") == 0) {
		if ((!flag && (argc<5)) || (flag && (argc != 9)))
		    printerrorexit();
		else {
		    xorig = chkgetarg(argv[++i],ORG);
		    yorig = chkgetarg(argv[++i],ORG);
		}
        	resetorg = TRUE;
		flag = TRUE;
	    } 
	} else if (!multiple_imgs) {
	    if ((argv[i]==NULL) || ((dash==TRUE)&&(flag==FALSE)) || (file))
		printerrorexit();
	    else {
		if ((open(argv[i],0) < 0)) {
	            fprintf(stderr,"bgpaste: %s is not an image file\n",argv[i]);
		    exit(0);
		}
		close(argv[i]);
		*fileindex = i;
		file = TRUE;
	    }
	}
    }               
    if ((dash==TRUE) && (flag==FALSE))
        printerrorexit();

}


chkgetarg(arg, type)
char *arg;
long type;
{
    long val;

    if (arg == NULL) {
	printerrorexit();
    } else if (type == RGB) {
	val=atoi(arg);
	if ((val<0) || (val>255)) {
	    fprintf(stderr,"Warning!  RGB value of %d is not in range of [0..255]\n",val);
	    printerrorexit();
        } else 
	    return (val);
    } else if (type == ORG) {
	val=atoi(arg);
    } else 
	printerrorexit();
}      


printerrorexit()
{
    fprintf(stderr,"usage: bgpaste [-t r g b] [-o xorig yorig] inimage\n");
    fprintf(stderr,"usage: bgpaste [-t r g b] -n numimgs xorg yorg img ");
    fprintf(stderr,"[xorg yorg img . . . ]\n");
    exit(1);
}
