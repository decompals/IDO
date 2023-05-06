/*
 *	mag - 
 *		Magnify pixels on the screen by some factor.  You can use
 *		mag <factor> to magnify by higher powers.
 *	
 *				Paul Haeberli - 1984
 *
 */
#include "stdio.h"
#include "gl.h"
#include "device.h"
#include "port.h"

#define NOSRC	(-1)

Cursor glass = {
	0x0002, 0x0007, 0x000F, 0x041E, 
	0x3B3C, 0x40F8, 0x4030, 0x8420, 
	0x8410, 0x8410, 0x9F10, 0x8430, 
	0xC420, 0x4060, 0x71C0, 0x1F00
};

unsigned long *pixbuf;
unsigned short *spixbuf;
long pixlongs = -1;
int factor = 6;
long dstxsize, dstysize;
long xorg, yorg;
int curx = NOSRC;
int cury = NOSRC;
int blacklines = 0;
int shaded = 0;
int rgbmode = 0;
int menu;

main(argc,argv)
int argc;
char **argv;
{
    if (argc>1)
	factor = atoi(argv[1]);
    if (factor<1) 
	factor = 1;
    minsize(135,135);
    winopen("mag");
    /* Call winconstraints so window can be shrunk down later. */
    winconstraints();
    glcompat(GLC_SOFTATTACH, TRUE);
    subpixel(0);
    wintitle("mag");
    if (getgdesc(GD_BITS_NORM_SNG_RED) == 0) {
	rgbmode = 0;
	menu = defpup("mag %t|grid off|grid on|mag");
    } else {
	rgbmode = 1;
	RGBmode();
	gconfig();
	menu = defpup("mag %t|grid off|grid on|interp|flat|mag");
    }
    defcursor(1,glass);
    curorigin(1,5,10);
    setcursor(1,RED,0xfff);
    qdevice(ESCKEY);
    qdevice(MENUBUTTON);
    qdevice(MIDDLEMOUSE);
    qdevice(LEFTMOUSE);
    qdevice(UPARROWKEY);
    qdevice(DOWNARROWKEY);
    makeframe(1);
    while (1) {
	getloc(&curx,&cury);
	readit(curx,cury);
    }
}

static rgbpack(c)
unsigned long c;
{
    int r, g, b;

    r = (c>>0)&0xff;
    g = (c>>8)&0xff;
    b = (c>>16)&0xff;
    return rgbi(r,g,b);
}

unsigned long *readscreen(x,y,xsize,ysize)
int x,y,xsize,ysize;
{
    int nlongs;

    nlongs = xsize*ysize;
    if(nlongs != pixlongs) {
	if(pixbuf)
	     free(pixbuf);
	pixlongs = nlongs;
	pixbuf = (unsigned long *)malloc(pixlongs*sizeof(long));
    }
    readdisplay(x,y,x+xsize-1,y+ysize-1,pixbuf,0); 
    return pixbuf;
}

readit(x,y)
int x, y;
{
    int srcxsize, srcysize;
    int i;
    int srcxorg, srcyorg;
    float xtrans, ytrans;
    unsigned long *ltemp, *dbuf;
    unsigned long *pbuf0, *pbuf1;

    srcxsize = dstxsize/factor+1; 
    srcxorg = x-srcxsize/2; 

    srcysize = dstysize/factor+1; 
    srcyorg = y-srcysize/2;

    percentdone(50.0);
    if (factor<5) {
	pbuf0 = readscreen(srcxorg,srcyorg,srcxsize,srcysize);
	dbuf = (unsigned long *)malloc((1+srcxsize*factor)*sizeof(long));
	for (i=0; i<dstysize; i++) {
	    if ((i%factor) == 0) {
		expandlongs(pbuf0,dbuf,srcxsize,factor);
		pbuf0 += srcxsize;
		if (blacklines && factor>2) {
		    grey(0.0);
		    move2i(0,i);
		    draw2i(dstxsize,i);
		} else {
		    putscanline(0,i,dstxsize,dbuf);
		}
	    } else {
		putscanline(0,i,dstxsize,dbuf);
	    }
	}
	free(dbuf);
    } else {
	if (!shaded) {
	    xtrans = 0;
	    ytrans = 0;
	    pbuf0 = readscreen(srcxorg,srcyorg,srcxsize,srcysize);
	    for (i=0; i<dstysize; i+=factor) {
		pushmatrix();
		    translate(xtrans,ytrans+i,0.0);
		    if(rgbmode) 
		        drawrectsrgb(pbuf0,srcxsize,factor);
		    else
		        fakedrawrectsrgb(pbuf0,srcxsize,factor);
		popmatrix();
		pbuf0 += srcxsize;
	    }
	} else {
	    xtrans = -(factor>>1);
	    ytrans = -(factor>>1);
	    pbuf0 = readscreen(srcxorg-1,srcyorg-1,srcxsize+2,srcysize+2);
	    pbuf1 = pbuf0+srcxsize+2;
	    for (i=0; i<dstysize+factor; i+=factor) {
		pushmatrix();
		    translate(xtrans,ytrans+i,0.0);
		    drawsrectsrgb(pbuf0,pbuf1,srcxsize+2,factor);
		popmatrix();
		pbuf0 = pbuf1;
		pbuf1 = pbuf0+srcxsize+2;
	    }
	}
    }
    setcursor(1,RED,0xfff);
}

putscanline(x,y,n,buf)
int x, y, n;
unsigned long *buf;
{
    int i, r, g, b;

    if(rgbmode) {
	lrectwrite(x,y,x+n-1,y,buf);
    } else {
	if(!spixbuf)
	    spixbuf = (unsigned short *)malloc(getgdesc(GD_XPMAX)*sizeof(short));
	for(i=0; i<n; i++) {
	    r = (*buf>>0)&0xff;
	    g = (*buf>>8)&0xff;
	    b = (*buf>>16)&0xff;
	    spixbuf[i] = rgbi(r,g,b);
	}
	cmov2i(x,y);
	writepixels(n,spixbuf);
    }
}

drawrectsrgb(buf,len,size)
unsigned long *buf;
int len, size;
{
    int x, nx;

    if (blacklines) {
	x = 0;
	cpack(0);
	myfillrecti(x,0,size*len,size);
	size -= 1;
	while (len--) {
	    cpack(*buf++);
	    nx = x+size;
	    myfillrecti(x,0,nx,size);
	    x = nx+1;
	}
    } else {
	x = 0;
	while (len--) {
	    cpack(*buf++);
	    nx = x+size;
	    myfillrecti(x,0,nx,size);
	    x = nx;
	}
    }
}

fakedrawrectsrgb(buf,len,size)
unsigned long *buf;
int len, size;
{
    int x, nx;

    if (blacklines) {
	x = 0;
	rgbpack(0);
	myfillrecti(x,0,size*len,size);
	size -= 1;
	while (len--) {
	    cpack(*buf++);
	    nx = x+size;
	    myfillrecti(x,0,nx,size);
	    x = nx+1;
	}
    } else {
	x = 0;
	while (len--) {
	    cpack(*buf++);
	    nx = x+size;
	    myfillrecti(x,0,nx,size);
	    x = nx;
	}
    }
}

drawsrectsrgb(buf0,buf1,len,size)
unsigned long *buf0, *buf1;
int len, size;
{
    int x, nx;
    float p[2];

    len--;
    if (blacklines) {
	x = 0;
	grey(0.0);
	myfillrecti(x,0,size*len,size);
	size -= 1;
	while (len--) {
	    nx = x+size;
	    bgnpolygon();
	      cpack(*buf0++);
	      p[0] = x-0.5; p[1] = -0.5;
	      v2f(p);
	      cpack(*buf1++);
	      p[1] = size-0.5;
	      v2f(p);
	      cpack(*buf1);
	      p[0] = nx-0.5;
	      v2f(p);
	      cpack(*buf0);
	      p[1] = -0.5;
	      v2f(p);
	    endpolygon();
	    x = nx+1;
	}
    } else {
	x = 0;
	while (len--) {
	    nx = x+size;
	    bgnpolygon();
	      cpack(*buf0++);
	      p[0] = x-0.5; p[1] = -0.5;
	      v2f(p);
	      cpack(*buf1++);
	      p[1] = size+0.5;
	      v2f(p);
	      cpack(*buf1);
	      p[0] = nx-0.5;
	      v2f(p);
	      cpack(*buf0);
	      p[1] = 0.5;
	      v2f(p);
	    endpolygon();
	    x = nx;
	}
    }
}

expandlongs(buf,dbuf,n,factor)
unsigned long *buf, *dbuf;
int n, factor;
{
    unsigned long *sptr, *dptr;
    short j;

    sptr = &buf[n];
    dptr = &dbuf[n*factor];
    while (sptr != buf) {
	sptr--;
	for (j=0; j<factor; j++) 
	    *--dptr =  *sptr;
    }
    if (blacklines && factor>2) {
	dptr = dbuf;
	for (j=0; j<n; j++) {
	    *dptr = 0;
	    dptr += factor;
	}
    }
}

getloc( x, y )
int *x, *y;
{
    short dev, val;
    short gotit = 0;
    int mx, my;

    curson();
    qreset();
    while (1) {
	dev = qread(&val);
	switch (dev) {
	    case ESCKEY:
		exit(0);
		break;
	    case MIDDLEMOUSE:
		if (val) {
		    mx = getvaluator(MOUSEX);
		    my = getvaluator(MOUSEY);
		    if (mx > xorg && mx < xorg+dstxsize &&
			       my > yorg && my < yorg+dstysize ) {
			factor = (60*(mx-xorg))/dstxsize;
			if (factor<2) 
			    factor = 2;
			makeframe(0);
		    }
		}
		break;
	    case LEFTMOUSE:
		if ((val == 1)) {
		    *x = getvaluator(MOUSEX);
		    *y = getvaluator(MOUSEY);
		    gotit++;
		} else if (gotit) {
		    return;
		}
		break;
	    case REDRAW:
		makeframe(0);
		break;
	    case DOWNARROWKEY:
		if(val) {
		    if (factor>2) {
			factor--;
			makeframe(0);
		    }
		}
		break;
	    case UPARROWKEY:
		if(val) {
		    factor++;
		    makeframe(0);
		}
		break;
	    case MENUBUTTON:
		if (val) {
		    if(rgbmode) { 
			switch(dopup(menu)) {
			    case 1:
				blacklines = 0;
				makeframe(0);
				break;
			    case 2:
				blacklines = 1;
				makeframe(0);
				break;
			    case 3:
				shaded = 1;
				makeframe(0);
				break;
			    case 4:
				shaded = 0;
				makeframe(0);
				break;
			    case 5:
				dosystem("mag");
				break;
			}
		    } else  {
			switch(dopup(menu)) {
			    case 1:
				blacklines = 0;
				makeframe(0);
				break;
			    case 2:
				blacklines = 1;
				makeframe(0);
				break;
			    case 3:
				dosystem("mag");
				break;
			}
		    }
		    setcursor(1,RED,0xfff);
		}
		break;
	}
    }
}

makeframe(doclear)
int doclear;
{
    reshapeviewport();
    getorigin(&xorg,&yorg);
    getsize(&dstxsize,&dstysize);
    ortho2(-0.5,dstxsize-0.5,-0.5,dstysize-0.5);
    dstxsize++;
    dstysize++;
    if ((curx == NOSRC) || doclear) {
	grey(0.5);
	clear();
    }
    if(curx != NOSRC) 
 	readit(curx,cury); 
}

myfillrecti(x1,y1,x2,y2)
int x1,y1,x2,y2;
{
    float v[2];

    bgnpolygon();
	v[0] = x1-0.5;
	v[1] = y1-0.5;
	v2f(v);
	v[0] = x2-0.5;
	v2f(v);
	v[1] = y2-0.5;
	v2f(v);
	v[0] = x1-0.5;
	v2f(v);
    endpolygon();
}

