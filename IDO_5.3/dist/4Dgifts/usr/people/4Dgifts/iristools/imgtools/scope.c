/*
 *	scope - 
 *		Examine an image up close and personal.
 *
 * 			    Paul Haeberli - 1988
 */
#include "math.h"
#include "gl.h"
#include "device.h"
#include "image.h"
#include "rct.h"

#define BORDER	3

rct *textline;
rct *zoompic;
rct *wholepic;
IMAGE *image;
int xpos, ypos;
int cr, cg, cb;
int zfactor;
int ixsize, iysize;
int ixorg, iyorg;

unsigned short hashmarks[16] = {
    0xcccc, 0xcccc, 0x6666, 0x6666, 0x3333, 0x3333, 0x9999, 0x9999,
    0xcccc, 0xcccc, 0x6666, 0x6666, 0x3333, 0x3333, 0x9999, 0x9999,
};

short rbuf[8192];
short gbuf[8192];
short bbuf[8192];

main(argc,argv)
int argc;
char **argv;
{
    char name[256];
    short val;
    int x, y, ox, oy;
    int oxpos, oypos;

    if( argc<2 ) {
	fprintf(stderr,"usage: scope inimage\n");
	exit(1);
    } 
    image = iopen(argv[1],"r");
    if(!image ) {
	fprintf(stderr,"scope: can't open input image\n");
	exit(1);
    }
    textline = rctnew();
    zoompic = rctnew();
    wholepic = rctnew();
    zfactor = 10;
    winopen("scope");
    xpos = image->xsize/2;
    ypos = image->ysize/2;
    sprintf(name,"scope %s\n",argv[1]);
    wintitle(name);
    qmouse();
    qdevice(UPARROWKEY);
    qdevice(DOWNARROWKEY);
    defpattern(2,16,hashmarks);
#ifdef DOUBLEBUFFER
    doublebuffer();
#endif
    RGBmode();
    gconfig();
#ifdef DOUBLEBUFFER
    frontbuffer(1);
#endif
    makeframe();
    movepos(image->xsize/2,image->ysize/2);
    while(1) {
	switch(qread(&val)) {
	    case REDRAW:
		makeframe();
		break;
	    case UPARROWKEY:
		if(val) 
		    zoomup();
		break;
	    case DOWNARROWKEY:
		if(val) 
		    zoomdown();
		break;
	    case LEFTMOUSE:
		if(val)  {
		    x = getmousex();
		    y = getmousey();
		    if(inrct(wholepic,x,y)) {
		        while(getbutton(LEFTMOUSE)) {
			    x = getmousex();
			    y = getmousey();
			    wintoimage(x,y,&x,&y);
			    movepos(x,y);
		        }
		    } else if(inrct(zoompic,x,y)) {
			ox = x;
			oy = y;
			oxpos = xpos;
			oypos = ypos;
		        while(getbutton(LEFTMOUSE)) {
			    x = getmousex();
			    y = getmousey();
			    movepos(oxpos+(ox-x)/zfactor,oypos+(oy-y)/zfactor);
			}
		    }
		}
		break;
	}
    }
}

makeframe()
{
    long xsize, ysize;

    reshapeviewport();
    getsize(&xsize,&ysize);
    grey(0.5);
    clear();
    rctset(textline,0,0,xsize,16); 
    rctset(zoompic,BORDER,16+BORDER,xsize/2-BORDER,ysize-BORDER); 
    rctset(wholepic,xsize/2+BORDER,16+BORDER,xsize-BORDER,ysize-BORDER); 
    drawtextline();
    drawwholepic();
    drawzoompic();
}

zoomup()
{
    zfactor++;
    drawzoompic();
    drawtextline();
}

zoomdown()
{
    if(zfactor>3) {
	zfactor--;
	drawzoompic();
	drawtextline();
    }
}

movepos(x,y)
int x, y;
{
    if(x>=(signed int)image->xsize)
	x = image->xsize-1;
    if(x<0)
	x = 0;
    if(y>=(signed int)image->ysize)
	y = image->ysize-1;
    if(y<0)
	y = 0;
    if(x == xpos && y == ypos)
	return;
    xpos = x;
    ypos = y;
    if(image->zsize<3) {
	getrow(image,rbuf,y,0);
	cr = cg = cb = rbuf[x];
    } else {
	getrow(image,rbuf,y,0);
	getrow(image,gbuf,y,1);
	getrow(image,bbuf,y,2);
	cr = rbuf[x];
	cg = gbuf[x];
	cb = bbuf[x];
    }
    drawtextline();
    drawzoompic();
}

drawtextline()
{
    char str[256];


    rctviewport(textline);
    sprintf(str,"xpos: %04d ypos: %04d   color: %3d %3d %3d   zoom: %d",xpos,ypos,cr,cg,cb,zfactor);
    grey(0.8);
    clear();
    grey(0.0);
    cmov2i(textline->xmin+10,textline->ymin+4);
    charstr(str);
}

drawzoompic()
{
    int x, y, xpix, ypix;
    int nx, ny, xstart;
    int xmax, ymax;
    int wxsize, wysize;
    int xorg, yorg;
    int xcorner, ycorner;

#ifdef DOUBLEBUFFER
    frontbuffer(0);
#endif
    rctviewport(zoompic);
    wxsize = zoompic->xmax-zoompic->xmin;
    wysize = zoompic->ymax-zoompic->ymin;
    nx = (3+wxsize/zfactor)|1;
    ny = (3+wysize/zfactor)|1;
    xmax = xpos+nx/2;
    ymax = ypos+ny/2;
    xcorner = wxsize/2-(nx*zfactor)/2;
    ycorner = wysize/2-(ny*zfactor)/2;
    yorg = ycorner;
    for(y=ypos-ny/2; y<ymax; y++) {
	xorg = xcorner;
	xstart = xpos-nx/2;
	xmax = xpos+nx/2;
	if(y<0 || y>=image->ysize) {
	    drawhashrow(xorg,yorg,xstart,xmax);
	} else {
	    if(image->zsize<3) {
		getrow(image,rbuf,y,0);
	    } else {
		getrow(image,rbuf,y,0);
		getrow(image,gbuf,y,1);
		getrow(image,bbuf,y,2);
	    }
	    if(xstart<0 || xmax>image->xsize) {
		drawhashrow(xorg,yorg,xstart,xmax);
		if(xstart<0) {
		    xorg -= xstart*zfactor;
		    xstart = 0;
		}
		if(xmax>image->xsize)
		    xmax = image->xsize;
	    }
	    for(x=xstart; x<xmax; x++) {
		if(image->zsize<3) {
		    RGBcolor(rbuf[x],rbuf[x],rbuf[x]);
		} else {
		    RGBcolor(rbuf[x],gbuf[x],bbuf[x]);
		}
		rectfi(xorg,yorg,xorg+zfactor,yorg+zfactor);
		xorg += zfactor;
            }
	}
	yorg += zfactor;
    } 
    xorg = xcorner + (nx/2)*zfactor;
    yorg = ycorner + (ny/2)*zfactor;
    if(cg>128)
	RGBcolor(0,0,0);
    else
	RGBcolor(255,255,255);
    recti(xorg,yorg,xorg+zfactor-1,yorg+zfactor-1);
#ifdef DOUBLEBUFFER
    swapbuffers();
    frontbuffer(1);
#endif
}

drawhashrow(xorg,yorg,xstart,xmax)
int xorg,yorg,xstart,xmax;
{
    RGBcolor(0,0,0);
    rectfi(xorg,yorg,xorg+(xmax-xstart)*zfactor,yorg+zfactor);
    setpattern(2);
    RGBcolor(255,255,255);
    rectfi(xorg,yorg,xorg+(xmax-xstart)*zfactor,yorg+zfactor);
    setpattern(0);
}

drawwholepic()
{
    int wxsize, wysize;
    float waspect, iaspect;

    rctviewport(wholepic);
    wxsize = wholepic->xmax-wholepic->xmin;
    wysize = wholepic->ymax-wholepic->ymin;
    waspect = (float)wysize/wxsize;
    iaspect = (float)image->ysize/image->xsize;
    if(iaspect>waspect) {
	iysize = wysize;
	ixsize = wysize/iaspect+0.5;
	ixorg = (wxsize-ixsize)/2;
	iyorg = 0;
    } else {
	ixsize = wxsize;
	iysize = iaspect*wxsize+0.5;
	iyorg = (wysize-iysize)/2;
	ixorg = 0;
    }
    pushmatrix();
    translate((float)ixorg,(float)iyorg,0.0);
    impulsezoom(image->xsize,image->ysize,ixsize,iysize);
    popmatrix();
}

wintoimage(sx,sy,ix,iy)
int sx, sy;
int *ix, *iy;
{
    sx -= (wholepic->xmin+ixorg);
    sy -= (wholepic->ymin+iyorg);
    *ix = ((signed int)image->xsize*sx)/ixsize;
    *iy = ((signed int)image->ysize*sy)/iysize;
}

rctviewport(r)
rct *r;
{
    viewport(r->xmin,r->xmax,r->ymin,r->ymax);
    ortho2(-0.5,r->xmax-r->xmin+0.5,-0.5,r->ymax-r->ymin+0.5);
}

inrct(r,x,y)
rct *r;
int x, y;
{
    if(x<r->xmin)
	return 0;
    if(x>r->xmax)
	return 0;
    if(y<r->ymin)
	return 0;
    if(y>r->ymax)
	return 0;
    return 1;
}

/* impulse zoom implementation follows */

#define GRIDTOFLOAT(pos,n)	(((pos)+0.5)/(n))
#define FLOATTOGRID(pos,n)	((pos)*(n))

static makexmap(abuf,xmap,anx,bnx,flip)
short *abuf;
short *xmap[];
int anx, bnx, flip;
{
    int x, xp, ax;
    float fx;

    for(x=0; x<bnx; x++) {
        if(flip&1)
            xp = (bnx-1)-x;
	else
            xp = x;
        fx = GRIDTOFLOAT(xp,bnx);
        ax = FLOATTOGRID(fx,anx);
        xmap[x] = abuf+ax;
    }
}

static xscalebuf(xmap,bbuf,bnx)
register short *xmap[];
register char *bbuf;
register int bnx;
{

    while(bnx--) {
	if(bnx>=7) {
	    *bbuf++ = *(*xmap++);
	    *bbuf++ = *(*xmap++);
	    *bbuf++ = *(*xmap++);
	    *bbuf++ = *(*xmap++);
	    *bbuf++ = *(*xmap++);
	    *bbuf++ = *(*xmap++);
	    *bbuf++ = *(*xmap++);
	    *bbuf++ = *(*xmap++);
	    bnx-=7;
        } else
	    *bbuf++ = *(*xmap++);
    }
}

impulsezoom(anx,any,bnx,bny)
int anx, any, bnx, bny;
{
    int x, y;
    int curay, ay;
    short *rabuf, *gabuf, *babuf;
    unsigned char *rbbuf, *gbbuf, *bbbuf;
    short **rxmap, **gxmap, **bxmap;
    float fy;

    rabuf = (short *)malloc(anx*sizeof(short));
    gabuf = (short *)malloc(anx*sizeof(short));
    babuf = (short *)malloc(anx*sizeof(short));
    rbbuf = (unsigned char *)malloc((bnx*sizeof(short)));
    gbbuf = (unsigned char *)malloc((bnx*sizeof(short)));
    bbbuf = (unsigned char *)malloc((bnx*sizeof(short)));
    rxmap = (short **)malloc(bnx*sizeof(short *));
    gxmap = (short **)malloc(bnx*sizeof(short *));
    bxmap = (short **)malloc(bnx*sizeof(short *));
    makexmap(rabuf,rxmap,anx,bnx,0);
    makexmap(gabuf,gxmap,anx,bnx,0);
    makexmap(babuf,bxmap,anx,bnx,0);
    curay = -1;
    for(y=0; y<bny; y++) {
	fy = GRIDTOFLOAT(y,bny);
	ay = FLOATTOGRID(fy,any);
	if(curay != ay) {
	    if(image->zsize<3) {
		getrow(image,rabuf,ay,0);
		xscalebuf(rxmap,rbbuf,bnx);
	    } else {
		getrow(image,rabuf,ay,0);
		getrow(image,gabuf,ay,1);
		getrow(image,babuf,ay,2);
		xscalebuf(rxmap,rbbuf,bnx);
		xscalebuf(gxmap,gbbuf,bnx);
		xscalebuf(bxmap,bbbuf,bnx);
	    }
	    curay = ay;
	}
	cmov2i(0,y);
	if(image->zsize<3) 
	    writeRGB(bnx,rbbuf,rbbuf,rbbuf);
	else
	    writeRGB(bnx,rbbuf,gbbuf,bbbuf);
    }
    free(rabuf);
    free(gabuf);
    free(babuf);
    free(rbbuf);
    free(gbbuf);
    free(bbbuf);
    free(rxmap);
    free(gxmap);
    free(bxmap);
}
