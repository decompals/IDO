/*
 *	setscreen - 
 *		PostScript style halftoning stuff
 *
 *			Paul Haeberli - 1990
 */
#include "values.h"
#include "math.h"
#include "stdio.h"
#include "setscreen.h"

static findindex();

#define MAXBMSIZE	(100000)
#define PRINTRES	(300.0)

#define OCT0	0
#define OCT1	1

static float *fbm;
static int bm1size, bm2size;

static int cmpfunc(v0,v1)
int *v0, *v1;
{
    if(fbm[*v0]<fbm[*v1])
	return -1;
    if(fbm[*v0]>fbm[*v1])
	return 1;
    return 0;
}

float sampleproc(dx,dy,x,y,proc,rot90)
int dx, dy;
int x, y;
float (*proc)();
int rot90;
{
    float fx, fy, mag, temp;

    mag = bm1size+bm2size;
    fx = (dy*y+dx*x)/mag;
    fy = (dy*x-dx*y)/mag;
    if(fx<0.0)
	fx += 1.0;
    if(fy<0.0)
	fy += 1.0;
    if(fx>1.0)
	fx -= 1.0;
    if(fy>1.0)
	fy -= 1.0;
    if(rot90>=2) {
	fx = 1.0-fx;
	fy = 1.0-fy;
	rot90 -= 2;
    }
    if(rot90) {
 	temp = 1.0-fx;
	fx = fy;
	fy = temp;
    }
    return proc(fx,fy);
}

static freescreeen(ht)
halftone *ht;
{
    if(ht->bm) {
	free(ht->bm);
        free(ht->nexti);
    }
    free(ht);
}

halftone *setscreen(lines,angle,proc)
float lines, angle;
float (*proc)();
{
    int i, x, y, s, f, p, rot90;
    int *order;
    int totsize, dx, dy, w2;
    int octant, base, mint, maxt;
    unsigned char *bm;
    int *nexti;
    halftone *ht;

    ht = (halftone *)malloc(sizeof(halftone));
    ht->angle = angle;
    while(angle>360.0)
	angle -= 360.0;
    while(angle<0.0)
	angle += 360.0;
    rot90 = 0;
    while(angle>=90.0) {
	angle -= 90.0;
	rot90++;
    }
    dx = ((PRINTRES/lines)*sin(M_PI*angle/180.0))+0.5;
    dy = ((PRINTRES/lines)*cos(M_PI*angle/180.0))+0.5;
    if(dx<0 || dy<0) {
	fprintf(stderr,"hway: screen too fine\n");
	exit(1);
    }
    if(dy>dx) 
	octant = OCT0;
    else
	octant = OCT1;
    bm1size = dy*dy;
    bm2size = dx*dx;
    totsize = bm1size+bm2size;
    printf("octant %d rot90 %d dx %d dy %d nlevels %d\n",octant,rot90,dx,dy,totsize);
    w2 = dx+dy;
    if((totsize)>MAXBMSIZE) {
	fprintf(stderr,"hway: screen too coarse\n");
	exit(1);
    }
    fbm = (float *)malloc(totsize*sizeof(float));
    bm = (unsigned char *)malloc(totsize);
    order = (int *)malloc(totsize*sizeof(int));
    nexti = (int *)malloc(totsize*sizeof(int));
    p = 0;

    ht->totsize = totsize;
    ht->dx = dx;
    ht->dy = dy;
    ht->w2 = w2;
    ht->octant = octant;
    ht->bm = bm;
    ht->nexti = nexti;

    if(octant == 0) {
	base = dy*(dy-dx);
	ht->base = base;
	for(i=0; i<totsize; i++) {
	    if(i<base) {
		x = i%dy;
		y = i/dy;
	    } else {
		x = (i-base)%w2;
		y = (dy-dx)+(i-base)/w2;
	    }
	    fbm[i] = sampleproc(dx,dy,x,y,proc,rot90);
	    order[p++] = i;
	    nexti[i] = findindex(ht,x+1,y);
	}
    } else {
	base = dx*(dx-dy);
	ht->base = base;
	for(i=0; i<totsize; i++) {
	    if(i<base) {
		x = (i%dx)+dy;
		y = (i/dx)-(dx-dy);
	    } else {
		x = (i-base)%w2;
		y = (i-base)/w2;
	    }
	    fbm[i] = sampleproc(dx,dy,x,y,proc,rot90);
	    order[p++] = i;
	    nexti[i] = findindex(ht,x+1,y);
	}
    }
    qsort(order,bm1size+bm2size,sizeof(int),cmpfunc);
    mint = 256;
    maxt = 0;
    s = 0;
    for(i=0; i<256; i++) {
	f = (i*(totsize+1))/256;
	while(s<f) {
	    p = order[s];
	    bm[p] = 255-i;
	    if(bm[p]>maxt)
		maxt = bm[p];
	    if(bm[p]<mint)
		mint = bm[p];
	    s++;
	}
    }
    free(order);
    free(fbm);
    ht->mint = mint;
    ht->maxt = maxt;
    return ht;
}

screenrow(ht,buf,n,x,y)
halftone *ht;
short *buf;
int n, y;
{
    int i;
    unsigned char *bm;
    int *nexti;

    i = findindex(ht,x,y);
    bm = ht->bm;
    nexti = ht->nexti;
    while (n--) {
	if(*buf>=bm[i])
	    *buf = 255;		
	else
	   *buf = 0;		
	i = nexti[i];
	buf++;
	x++;
    }
}

bmscreenrow(ht,buf,bits,n,x,y)
halftone *ht;
short *buf;
unsigned char *bits;
int n, y;
{
    int i;
    unsigned char c, maskbit;
    unsigned char *bm;
    int *nexti;

    i = findindex(ht,x,y);
    bm = ht->bm;
    nexti = ht->nexti;
    maskbit = 0x80;
    c = 0;
    while (n) {
	if(n>=8) {
	    if(buf[0]<bm[i])
		c  |= 0x80;
	    i = nexti[i];
	    if(buf[1]<bm[i])
		c  |= 0x40;
	    i = nexti[i];
	    if(buf[2]<bm[i])
		c  |= 0x20;
	    i = nexti[i];
	    if(buf[3]<bm[i])
		c  |= 0x10;
	    i = nexti[i];
	    if(buf[4]<bm[i])
		c  |= 0x08;
	    i = nexti[i];
	    if(buf[5]<bm[i])
		c  |= 0x04;
	    i = nexti[i];
	    if(buf[6]<bm[i])
		c  |= 0x02;
	    i = nexti[i];
	    if(buf[7]<bm[i])
		c  |= 0x01;
	    i = nexti[i];
	    buf += 8;
	    *bits++ = c;
	    n -= 8;
	    c = 0;
	} else {
	    if(*buf++<bm[i])
		c |= maskbit;
	    n--;
	}
    }
    if(maskbit != 0x80)
	*bits++ = c;
}

bmscreenlevel1(ht,level,bits,x,y)
halftone *ht;
unsigned char level;
unsigned char *bits;
int x, y;
{
    if(level>=ht->maxt)
	*bits &= (0xff7f>>(x&7));
    else if(level<ht->mint)
	*bits |= (0x80>>(x&7));
    else if(level<ht->bm[findindex(ht,x,y)])
	*bits |= (0x80>>(x&7));
    else
	*bits &= (0xff7f>>(x&7));
}

bmscreenlevel(ht,level,bits,n,x,y)
halftone *ht;
unsigned char level;
unsigned char *bits;
int n, x, y;
{
    int i, low;
    unsigned char c, maskbit;
    unsigned char *bm;
    int *nexti;

    low = x&7;
    if(level>=ht->maxt) {
	if(low) {
	    if(n<=(8-low)) {
		maskbit = (0xff>>low)&(0xff00>>(low+n));
		*bits &= ~maskbit;
		return;
	    }
	    n -= 8-low;
	    *bits++ &= ~(0x00ff>>low);
	}
	while (n>=64) {
	    *bits++ = 0x00;
	    *bits++ = 0x00;
	    *bits++ = 0x00;
	    *bits++ = 0x00;
	    *bits++ = 0x00;
	    *bits++ = 0x00;
	    *bits++ = 0x00;
	    *bits++ = 0x00;
	    n -= 64;
	}
	while (n>=8) {
	    *bits++ = 0x00;
	    n -= 8;
	}
	if(n>0) 
	    *bits &= (0x00ff>>n);
	return;
    } else if(level<ht->mint) {
	if(low) {
	    if(n<=(8-low)) {
		maskbit = (0xff>>low)&(0xff00>>(low+n));
		*bits |= maskbit;
		return;
	    }
	    n -= 8-low;
	    *bits++ |= (0x00ff>>low);
	}
	while (n>=64) {
	    *bits++ = 0xff;
	    *bits++ = 0xff;
	    *bits++ = 0xff;
	    *bits++ = 0xff;
	    *bits++ = 0xff;
	    *bits++ = 0xff;
	    *bits++ = 0xff;
	    *bits++ = 0xff;
	    n -= 64;
	}
	while (n>=8) {
	    *bits++ = 0xff;
	    n -= 8;
	}
	if(n>0) 
	    *bits |= (0xff00>>n);
	return;
    }
    i = findindex(ht,x,y);
    bm = ht->bm;
    nexti = ht->nexti;
    if(low) {
	c = *bits;
	maskbit = 0x80>>low;
	while(n) {
	    if(level<bm[i])
		c |= maskbit;
	    else
		c &= ~maskbit;
	    i = nexti[i];
	    maskbit >>= 1;
	    n--;
	    if(maskbit == 0)
	       break;
	}
	*bits++ = c;
    }
    while (n>=8) {
	c = 0;
	if(level<bm[i])
	    c  |= 0x80;
	i = nexti[i];
	if(level<bm[i])
	    c  |= 0x40;
	i = nexti[i];
	if(level<bm[i])
	    c  |= 0x20;
	i = nexti[i];
	if(level<bm[i])
	    c  |= 0x10;
	i = nexti[i];
	if(level<bm[i])
	    c  |= 0x08;
	i = nexti[i];
	if(level<bm[i])
	    c  |= 0x04;
	i = nexti[i];
	if(level<bm[i])
	    c  |= 0x02;
	i = nexti[i];
	if(level<bm[i])
	    c  |= 0x01;
	i = nexti[i];
	*bits++ = c;
	n -= 8;
    } 
    if(n>0) {
	c = *bits;
	maskbit = 0x80;
	while (n) {
	    if(level<bm[i])
		c |= maskbit;
	    else
		c &= ~maskbit;
	    i = nexti[i];
	    maskbit >>= 1;
	    n--;
	}
	*bits = c;
    }
}

printht(ht)
halftone *ht;
{
    float aa;
    int start, repn;

    aa = (180.0/M_PI)*atan2((float)ht->dy,(float)ht->dx);
    printf("angle requested: %f  actual angle: %f\n",ht->angle,aa);
    printf("dx: %d dy: %d totsize: %d\n",ht->dx,ht->dy,ht->totsize);
    repn = 0;
    start = findindex(ht,0,0);
    repn++;
    while(findindex(ht,repn,0) != start)
	repn++;
    printf("cell size is %d by %d or %d area\n",repn,repn,repn*repn);
}

static findindex(ht,x,y)
halftone *ht;
int x, y;
{
    int nx, ny;
    int sx, sy;
    int bign, bigmag;
    int dx, dy;

    dx = ht->dx;
    dy = ht->dy;
    bign = 0x4000000/ht->totsize;
    bigmag = bign*ht->totsize;
    nx = (bigmag+dx*x+dy*y)/ht->totsize;
    nx -= bign;
    ny = (bigmag+dy*x-dx*y)/ht->totsize;
    ny -= bign;

    x = x-nx*dx-ny*dy;
    y = y-nx*dy+ny*dx;
    if(x<dy) {
	if(y<0) {
	    x = x+dx;
	    y = y+dy;
	}
    } else {
	if(y<(dy-dx)) {
	    x = x-dy;
	    y = y+dx;
	}
    }
    if(ht->octant == 0) {
	if(y<(dy-dx)) {
	    return y*dy+x;
	} else  {
	    y = y-(dy-dx);
	    return ht->base+y*ht->w2+x;
	}
    } else {
	if(y>=0) {
	    return ht->base+y*ht->w2+x;
	} else  {
	    x = x-dy;
	    y = y+(dx-dy);
	    return y*dx+x;
	}
    }
}
