/*
 *	cmyk -
 *		functions to support CMYK representations
 *
 *			   Paul Haeberli - 1990
 */
#include "math.h"
#include "stdio.h"
#include "image.h"
#include "lum.h"

/*
 *	rgb_to_cmyk -
 *		Convert from rgb to cmyk.  This implements grey component
 *	replacement. 
 *
 *	Inputs:
 *		r intensity 0 to 255
 *		g intensity 0 to 255
 *		b intensity 0 to 255
 *
 *	Outputs:
 *		c coverage 0 to 255
 *		m coverage 0 to 255
 *		y coverage 0 to 255
 *		k coverage 0 to 255
 *
 *
 *	An input value of rgb=[255,255,255] represents white.  When this is
 *	transformed to cmyk, we get cmyk=[0,0,0,0] which represents zero 
 *	coverage. 
 *
 *	An input value of rgb=[128,128,128] represents 50 percent grey.  When
 *	this is transformed to cmyk, we get cmyk=[0,0,0,127] which represents 
 *	zero coverage by cmy, and 50 percent coverage by black. 
 *
 */
static int ultrac = 160;
static int ultram = 160;
static int ultray = 0;
static int ultrak = 255;
static int blacklerp = 4;

cmyksetup(uc,um,uy,uk,bl)
int uc, um, uy, uk, bl;
{
    ultrac = uc;
    ultram = um;
    ultray = uy;
    ultrak = uk;
    blacklerp = bl;
}

vrgb_to_cmyk(rbuf,gbuf,bbuf,cbuf,mbuf,ybuf,kbuf,cortab,n)
short *rbuf,*gbuf,*bbuf;
short *cbuf,*mbuf,*ybuf,*kbuf;
short *cortab;
int n;
{
    int c, m, y, k;

    while(n--) {
	rgb_to_cmyk(*rbuf++,*gbuf++,*bbuf++,&c,&m,&y,&k,cortab);
	*cbuf++ = c;
	*mbuf++ = m;
	*ybuf++ = y;
	*kbuf++ = k;
    }
}

rgb_to_cmyk(r,g,b,c,m,y,k,cortab)
register int r, g, b;
int *c, *m, *y, *k;
short cortab[256];
{
    register int i;
    float param; 

/* i is the max of r, g, and b */
    i = 0;
    if(r>i)
	i = r;
    if(g>i)
	i = g;
    if(b>i)
	i = b;

/* if r, g and b are all zero then print full k plus some m and c */
    if(i == 0) {
	*c = ultrac;
	*m = ultram;
	*y = ultray;
	*k = ultrak;
	return;
    }
    r = (255*r)/i;
    g = (255*g)/i;
    b = (255*b)/i;
    if(cortab) {
	*c = 255-cortab[r];
	*m = 255-cortab[g];
	*y = 255-cortab[b];
	*k = 255-cortab[i];
    } else {
	*c = 255-r;
	*m = 255-g;
	*y = 255-b;
	*k = 255-i;
    }
	 
/* if i is less than blacklerp start lerp towards ultra-black */
    if(i<blacklerp) {
	param = ((float)i)/((float)blacklerp);
	*c = lerp(ultrac,*c,param);
	*m = lerp(ultram,*m,param);
	*y = lerp(ultray,*y,param);
	*k = *k;
    }
}

rgb_to_cmyonly(r,g,b,c,m,y,k,cortab)
register int r, g, b;
int *c, *m, *y, *k;
short cortab[256];
{
    if(cortab) {
	*c = 255-cortab[r];
	*m = 255-cortab[g];
	*y = 255-cortab[b];
    } else {
	*c = 255-r;
	*m = 255-g;
	*y = 255-b;
    }
    *k = 0;
}

rgb_to_konly(r,g,b,c,m,y,k,cortab)
register int r, g, b;
int *c, *m, *y, *k;
short cortab[256];
{
    int val; 

    val = ILUM(r,g,b);
    *c = 0;
    *m = 0;
    *y = 0;
    if(cortab)
	*k = 255-cortab[val];
    else
	*k = 255-val;
}

#define EPSILON	(0.0001)

/*
 *	frgb_to_cmyk -
 *		Convert from rgb to cmyk.  This implements grey component
 *	replacement. 
 *
 *	Inputs:
 *		r intensity 0 to 1.0
 *		g intensity 0 to 1.0
 *		b intensity 0 to 1.0
 *
 *	Outputs:
 *		c coverage 0 to 255
 *		m coverage 0 to 255
 *		y coverage 0 to 255
 *		k coverage 0 to 255
 *
 *
 *	An input value of rgb=[1.0,1.0,1.0] represents white.  When this is
 *	transformed to cmyk, we get cmyk=[0,0,0,0] which represents zero 
 *	coverage. 
 *
 *	An input value of rgb=[0.5,0.5,0.5] represents 50 percent grey.  When
 *	this is transformed to cmyk, we get cmyk=[0,0,0,127] which represents 
 *	zero coverage by cmy, and 50 percent coverage by black. 
 *
 */
frgb_to_cmyk(r,g,b,c,m,y,k)
register float r, g, b;
float *c, *m, *y, *k;
{
    register int i;
    float param; 

/* i is the max of r, g, and b */
    i = 0;
    if(r>i)
	i = r;
    if(g>i)
	i = g;
    if(b>i)
	i = b;

/* if r, g and b are all zero then print full k plus some m and c */
    if(i<EPSILON) {
	*c = ultrac;
	*m = ultram;
	*y = ultray;
	*k = ultrak;
	return;
    }
    r = (255*r)/i;
    g = (255*g)/i;
    b = (255*b)/i;
    *c = 255-r;
    *m = 255-g;
    *y = 255-b;
    *k = 255-i;

/* if i is less than BLACKLERP start lerp towards ultra-black */
    if(i<blacklerp) {
	param = ((float)i)/((float)blacklerp);
	*c = flerp((float)ultrac,*c,param);
	*m = flerp((float)ultram,*m,param);
	*y = flerp((float)ultray,*y,param);
	*k = *k;
    }
}

static unsigned short *rbuf, *gbuf, *bbuf;
static int bufxsize;

getcmykrow(image,cmykbuf,iy,z)
IMAGE *image;
unsigned short *cmykbuf;
int iy, z;
{
    int xsize, n;
    unsigned short *rptr, *gptr, *bptr;
    unsigned short *cmykptr;
    int c, m, y, k;

    xsize = image->xsize;
    if(!rbuf || xsize != bufxsize) {
	if(rbuf) {
	    free(rbuf);
	    free(gbuf);
	    free(bbuf);
	}
	rbuf = (unsigned short *)malloc(xsize*sizeof(short));
	gbuf = (unsigned short *)malloc(xsize*sizeof(short));
	bbuf = (unsigned short *)malloc(xsize*sizeof(short));
	bufxsize = xsize;
    }
    getrow(image,rbuf,iy,0%image->zsize);
    getrow(image,gbuf,iy,1%image->zsize);
    getrow(image,bbuf,iy,2%image->zsize);
    rptr = rbuf;
    gptr = gbuf;
    bptr = bbuf;
    cmykptr = cmykbuf;
    n = xsize;
    while(n--) {
	rgb_to_cmyk(*rptr++,*gptr++,*bptr++,&c,&m,&y,&k,0);
	switch(z) {
	    case 0:
		*cmykptr++ = c;
		break;
	    case 1:
		*cmykptr++ = m;
		break;
	    case 2:
		*cmykptr++ = y;
		break;
	    case 3:
		*cmykptr++ = k;
		break;
	}
    }
}
