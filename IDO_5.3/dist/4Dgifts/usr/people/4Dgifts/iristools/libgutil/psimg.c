/*
 *	psimg - 
 *		Convert image data into b/w or color PostScript.
 *
 *			Paul Haeberli - 1989
 *
 */
#include "image.h"
#include "math.h"

#define MAXWIDTH	8192

static short cortab[256];

static short buf[MAXWIDTH];

static short rbuf[MAXWIDTH];
static short gbuf[MAXWIDTH];
static short bbuf[MAXWIDTH];
static short rgbbuf[3*MAXWIDTH];

static short cbuf[MAXWIDTH];
static short mbuf[MAXWIDTH];
static short ybuf[MAXWIDTH];
static short kbuf[MAXWIDTH];
static short cmykbuf[4*MAXWIDTH];

static int pos;
static int psfirsted;

typedef struct printspec {
    float screendensity;
    float screenangle; 
    float xpixperinch; 
    float ypixperinch; 
    float scaletrim; 
    float maxxsize, maxysize;
    int bitsper, linescreen;
} printspec;

static IMAGE *inimage;
static int binaryout = 0;

static bwgetimgrow();
static rgbgetimgrow();
static hypcurve();
static applytable();
static putpsrow();
static putpsascrow();
static putpsbinrow();
static makepstables();
static psputchar();
static intervleave3();
static intervleave4();

usebinaryps(b)
int b;
{
    binaryout = b;
}

static bwgetimgrow(buf,y)
short *buf;
int y;
{
    getbwrow(inimage,buf,inimage->ysize-1-y);
}

static rgbgetimgrow(buf,y,z)
short *buf;
int y, z;
{
    getrow(inimage,buf,inimage->ysize-1-y,z%inimage->zsize);
}

frametobwps(outf,image) 
FILE *outf;
IMAGE *image;
{
    printspec ps;

    hypcurve(0.7);
    ps.screendensity = 60.0;
    ps.screenangle = 45.0;
    ps.xpixperinch = -1.0;
    ps.ypixperinch = -1.0;
    ps.scaletrim = 1.0;
    ps.bitsper = 8;
    ps.linescreen = 0;
    fprintf(outf,"\n translate scale\n");
    fprintf(outf,"pop pop\n");
    if(ps.linescreen) 
	fprintf(outf,"%f %f {pop} setscreen\n",ps.screendensity,ps.screenangle);
    else
        fprintf(outf,"%f %f {dup mul exch dup mul add 1 exch sub} setscreen\n",
					       ps.screendensity,ps.screenangle);
    inimage = image;
    tobwps(outf,bwgetimgrow,cortab,ps.bitsper,inimage->xsize,inimage->ysize);
}

frametorgbps(outf,image) 
FILE *outf;
IMAGE *image;
{
    printspec ps;

    hypcurve(0.7);
    ps.screendensity = 60.0;
    ps.screenangle = 45.0;
    ps.xpixperinch = -1.0;
    ps.ypixperinch = -1.0;
    ps.scaletrim = 1.0;
    ps.bitsper = 8;
    ps.linescreen = 0;
    fprintf(outf,"\n translate scale\n");
    fprintf(outf,"pop pop\n");
    if(ps.linescreen) 
        fprintf(outf,"%f %f {pop} setscreen\n",ps.screendensity,ps.screenangle);
    else
        fprintf(outf,"%f %f {dup mul exch dup mul add 1 exch sub} setscreen\n",
					       ps.screendensity,ps.screenangle);
    inimage = image;
    torgbps(outf,rgbgetimgrow,cortab,ps.bitsper,inimage->xsize,inimage->ysize,1);
}

static hypcurve(p)
float p;
{
    float b;
    float x, y;
    int i;

    b = (1.0/p) - p;
    for(i=0; i<256; i++) {
	x = i/255.0;
	y = 1.0-(1.0/((x*b)+p) - p)/b;
	cortab[i] = (255*y)+0.5;
    }
}

static applytable(buf,cortab,n)
short *buf;
short *cortab;
int n;
{
    if(cortab) {
       	while(n--) {
	    *buf = cortab[*buf];
	    buf++;
       	}
    }
}

/*
 *	convert an image into B/W postscript
 *
 *
 */
static int hi[256], low[256];

tobwps(outf,getfunc,cortab,bitsper,xsize,ysize)
FILE *outf;
int (*getfunc)();
short cortab[256];
int bitsper, xsize, ysize;
{
    int y, picstrlen;

    if(!psfirsted) {
	makepstables();
	psfirsted++;
    }
    picstrlen = xsize*bitsper;
    picstrlen = (picstrlen+7)/8;

/* allocate the pixel buffer */
    fprintf(outf,"/picstr %d string def\n",picstrlen);

    fprintf(outf,"%d %d %d\n",xsize,ysize,bitsper);
    fprintf(outf,"[%d 0 0 -%d 0 %d]\n",xsize,ysize,ysize); 
    if(binaryout) {
	fprintf(outf,"{currentfile picstr readstring pop}\n");
        fprintf(outf,"image\r");
    } else  {
	fprintf(outf,"{currentfile picstr readhexstring pop}\n");
    	fprintf(outf,"image\n");
    }

/* send out the picture */
    pos = 0;
    for( y=0; y<ysize; y++ ) {
	(getfunc)(buf,y);
	applytable(buf,cortab,xsize);
	putpsrow(outf,buf,picstrlen,bitsper);
    }
    fprintf(outf,"\n");
}

/*
 *	convert an image into rgb postscript
 *
 *
 */
torgbps(outf,getfunc,cortab,bitsper,xsize,ysize,nproc)
FILE *outf;
int (*getfunc)();
short cortab[256];
int bitsper, xsize, ysize, nproc;
{
    int y, z, picstrlen;

    if(!psfirsted) {
	makepstables();
	psfirsted++;
    }
    picstrlen = xsize*bitsper;
    picstrlen = (picstrlen+7)/8;

    if(nproc == 1) {
/* allocate the pixel buffer */
	fprintf(outf,"/istr %d string def\n",3*picstrlen);
	fprintf(outf,"%d %d %d\n",xsize,ysize,bitsper);
	fprintf(outf,"[%d 0 0 -%d 0 %d]\n",xsize,ysize,ysize); 
	if(binaryout) {
	    fprintf(outf,"{currentfile istr readstring pop}\n");
	    fprintf(outf,"false 3\n");
	    fprintf(outf,"colorimage\r");
	} else  {
	    fprintf(outf,"{currentfile istr readhexstring pop}\n");
	    fprintf(outf,"false 3\n");
	    fprintf(outf,"colorimage\n");
	}

/* send out the picture */
	pos = 0;
	for( y=0; y<ysize; y++ ) {
	    (getfunc)(rbuf,y,0);
	    applytable(rbuf,cortab,xsize);
	    (getfunc)(gbuf,y,1);
	    applytable(gbuf,cortab,xsize);
	    (getfunc)(bbuf,y,2);
	    applytable(bbuf,cortab,xsize);
	    intervleave3(rbuf,gbuf,bbuf,rgbbuf,xsize);
	    putpsrow(outf,rgbbuf,3*picstrlen,bitsper);
	}
    } else {
/* allocate the pixel buffer */
	fprintf(outf,"/rstr %d string def\n",picstrlen);
	fprintf(outf,"/gstr %d string def\n",picstrlen);
	fprintf(outf,"/bstr %d string def\n",picstrlen);

	fprintf(outf,"%d %d %d\n",xsize,ysize,bitsper);
	fprintf(outf,"[%d 0 0 -%d 0 %d]\n",xsize,ysize,ysize); 
	if(binaryout) {
	    fprintf(outf,"{currentfile rstr readstring pop}\n");
	    fprintf(outf,"{currentfile gstr readstring pop}\n");
	    fprintf(outf,"{currentfile bstr readstring pop}\n");
	    fprintf(outf,"true 3\n");
	    fprintf(outf,"colorimage\r");
	} else  {
	    fprintf(outf,"{currentfile rstr readhexstring pop}\n");
	    fprintf(outf,"{currentfile gstr readhexstring pop}\n");
	    fprintf(outf,"{currentfile bstr readhexstring pop}\n");
	    fprintf(outf,"true 3\n");
	    fprintf(outf,"colorimage\n");
	}

/* send out the picture */
	pos = 0;
	for( y=0; y<ysize; y++ ) {
	    (getfunc)(rbuf,y,0);
	    applytable(rbuf,cortab,xsize);
	    (getfunc)(gbuf,y,1);
	    applytable(gbuf,cortab,xsize);
	    (getfunc)(bbuf,y,2);
	    applytable(bbuf,cortab,xsize);
	    putpsrow(outf,rbuf,picstrlen,bitsper);
	    putpsrow(outf,gbuf,picstrlen,bitsper);
	    putpsrow(outf,bbuf,picstrlen,bitsper);
	}
    }
    fprintf(outf,"\n");
}

/*
 *	convert an image into cmyk postscript
 *
 *
 */
tocmykps(outf,getfunc,cortab,bitsper,xsize,ysize,nproc)
FILE *outf;
int (*getfunc)();
short cortab[256];
int bitsper, xsize, ysize, nproc;
{
    int y, z, picstrlen;

    if(!psfirsted) {
	makepstables();
	psfirsted++;
    }
    picstrlen = xsize*bitsper;
    picstrlen = (picstrlen+7)/8;

    if(nproc == 1) {
/* allocate the pixel buffer */
	fprintf(outf,"/istr %d string def\n",4*picstrlen);
	fprintf(outf,"%d %d %d\n",xsize,ysize,bitsper);
	fprintf(outf,"[%d 0 0 -%d 0 %d]\n",xsize,ysize,ysize); 
	if(binaryout) {
	    fprintf(outf,"{currentfile istr readstring pop}\n");
	    fprintf(outf,"false 4\n");
	    fprintf(outf,"colorimage\r");
	} else  {
	    fprintf(outf,"{currentfile istr readhexstring pop}\n");
	    fprintf(outf,"false 4\n");
	    fprintf(outf,"colorimage\n");
	}

/* send out the picture */
	pos = 0;
	for( y=0; y<ysize; y++ ) {
	    (getfunc)(rbuf,y,0);
	    (getfunc)(gbuf,y,1);
	    (getfunc)(bbuf,y,2);
	    vrgb_to_cmyk(rbuf,gbuf,bbuf,cbuf,mbuf,ybuf,kbuf,cortab,xsize);
	    intervleave4(cbuf,mbuf,ybuf,kbuf,cmykbuf,xsize);
	    putpsrow(outf,cmykbuf,4*picstrlen,bitsper);
	}
    } else {
/* allocate the pixel buffer */
	fprintf(outf,"/cstr %d string def\n",picstrlen);
	fprintf(outf,"/mstr %d string def\n",picstrlen);
	fprintf(outf,"/ystr %d string def\n",picstrlen);
	fprintf(outf,"/kstr %d string def\n",picstrlen);

	fprintf(outf,"%d %d %d\n",xsize,ysize,bitsper);
	fprintf(outf,"[%d 0 0 -%d 0 %d]\n",xsize,ysize,ysize); 

	if(binaryout) {
	    fprintf(outf,"{currentfile cstr readstring pop}\n");
	    fprintf(outf,"{currentfile mstr readstring pop}\n");
	    fprintf(outf,"{currentfile ystr readstring pop}\n");
	    fprintf(outf,"{currentfile kstr readstring pop}\n");
	    fprintf(outf,"true 4\n");
	    fprintf(outf,"colorimage\r");
	} else  {
	    fprintf(outf,"{currentfile cstr readhexstring pop}\n");
	    fprintf(outf,"{currentfile mstr readhexstring pop}\n");
	    fprintf(outf,"{currentfile ystr readhexstring pop}\n");
	    fprintf(outf,"{currentfile kstr readhexstring pop}\n");
	    fprintf(outf,"true 4\n");
	    fprintf(outf,"colorimage\n");
	}

/* send out the picture */
	pos = 0;
	for( y=0; y<ysize; y++ ) {
	    (getfunc)(rbuf,y,0);
	    (getfunc)(gbuf,y,1);
	    (getfunc)(bbuf,y,2);
	    vrgb_to_cmyk(rbuf,gbuf,bbuf,cbuf,mbuf,ybuf,kbuf,cortab,xsize);
	    putpsrow(outf,cbuf,picstrlen,bitsper);
	    putpsrow(outf,mbuf,picstrlen,bitsper);
	    putpsrow(outf,ybuf,picstrlen,bitsper);
	    putpsrow(outf,kbuf,picstrlen,bitsper);
	}
    }
    fprintf(outf,"\n");
}

static putpsrow(outf,buf,picstrlen,bitsper)
FILE *outf;
short *buf;
int picstrlen, bitsper;
{
    if(binaryout)
	putpsbinrow(outf,buf,picstrlen,bitsper);
    else
	putpsascrow(outf,buf,picstrlen,bitsper);
}

static putpsascrow(outf,buf,picstrlen,bitsper)
FILE *outf;
short *buf;
int picstrlen, bitsper;
{
    int x, n, i, val;

    switch(bitsper) {
	case 1:
	    x=0;
	    for(n=2*picstrlen; n--; ) {
		val = 0;
		for(i=0; i<4; i++) {
		    val <<= 1;
		    val |= (buf[x]&0x80) >> 7;
		    x++;
		}
		psputchar("0123456789abcdef"[val],outf);
	    }
	    break;	
	case 2:
	    x=0;
	    for(n=2*picstrlen; n--; ) {
		val = 0;
		for(i=0; i<2; i++) {
		    val <<= 2;
		    val |= (buf[x]&0xc0) >> 6;
		    x++;
		}
		psputchar("0123456789abcdef"[val],outf);
	    }
	    break;	
	case 4:
	    x=0;
	    for(n=2*picstrlen; n--; ) {
		val = (buf[x]&0xf0) >> 4;
		x++;
		psputchar("0123456789abcdef"[val],outf);
	    }
	    break;	
	case 8:
	    x=0;
	    for(n=2*picstrlen; n--; ) {
		val = buf[x];
		if(val > 255)
		    fprintf(stderr,"bad poop\n");
		x++;
		n--;
		psputchar(hi[val],outf);
		psputchar(low[val],outf);
	    }
	    break;
	default:
	    fprintf(stderr,"bits per pixel must be a power of 2!!\n");
	    exit(1);
    }
}

static putpsbinrow(outf,buf,picstrlen,bitsper)
FILE *outf;
short *buf;
int picstrlen, bitsper;
{
    int x, n, i, val;

    switch(bitsper) {
	case 8:
	    x=0;
	    for(n=picstrlen; n--;) {
		val = buf[x];
		if(val > 255)
		    fprintf(stderr,"bad poop\n");
		x++;
		fputc(val,outf);
	    }
	    break;
	default:
	    fprintf(stderr,"XXX BLAT bits per pixel must be 8\n");
	    exit(1);
    }
}

static makepstables()
{
    register int i;

    for(i=0; i<256; i++) {
	hi[i] = "0123456789abcdef"[i>>4];
	low[i] = "0123456789abcdef"[i&0xf];
    }
}

static psputchar(c,outf)
int c;
FILE *(outf);
{
    fputc(c,outf);
    if(++pos == 70) { 
	fputc('\n',outf);
	pos = 0;
    }
}

static intervleave3(rbuf,gbuf,bbuf,rgbbuf,n)
short *rbuf, *gbuf, *bbuf, *rgbbuf;
int n;
{
    while(n--) {
	*rgbbuf++ = *rbuf++;
	*rgbbuf++ = *gbuf++;
	*rgbbuf++ = *bbuf++;
    }
}

static intervleave4(cbuf,mbuf,ybuf,kbuf,cmykbuf,n)
short *cbuf, *mbuf, *ybuf, *kbuf, *cmykbuf;
int n;
{
    while(n--) {
	*cmykbuf++ = *cbuf++;
	*cmykbuf++ = *mbuf++;
	*cmykbuf++ = *ybuf++;
	*cmykbuf++ = *kbuf++;
    }
}
