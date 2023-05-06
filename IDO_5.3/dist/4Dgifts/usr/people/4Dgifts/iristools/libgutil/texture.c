/*
 *	texture -
 *		Support for software texture maps.
 *
 *				Paul Haeberli - 1988
 */
#include "values.h"
#include "texture.h"
#include "port.h"

static init_noise3();
static normalize();

static short rowbuf[4096];
float frand();
float fnoise3();

#define DEGTORAD(x)	(((x)*M_PI)/180.0)

/*
 *	Utility Functions 
 *
 */ 
float onewrap(v)
float v;
{
    if(v >= 0.0)
	return (v - ((int)v));
    v = -v;
    return 1.0-(v - ((int)v));
}

float ease(x)
float x;
{
    if(x<0.0)
	return 0.0;
    if(x>1.0)
	return 1.0;
    return (sin(DEGTORAD(180.0*(x-0.5)))+1.0)/2.0;
}

vcubewrap(v,vw)
vect *v, *vw;
{
    vw->x = onewrap(v->x);
    vw->y = onewrap(v->y);
    vw->z = onewrap(v->z);
}

vforceunit(v)
vect *v;
{
    if(v->x<0.0)
	v->x = 0.0;
    if(v->y<0.0)
	v->y = 0.0;
    if(v->z<0.0)
	v->z = 0.0;
    if(v->w<0.0)
	v->w = 0.0;
    if(v->x>1.0)
	v->x = 1.0;
    if(v->y>1.0)
	v->y = 1.0;
    if(v->z>1.0)
	v->z = 1.0;
    if(v->w>1.0)
	v->w = 1.0;
}

addnormals(a,b,c)
vect *a, *b, *c;
{
    vadd(a,b,c);
    vnormal(c);
}

/*
 * 	Texture mapping support 
 *
 */
TEXTURE *tmalloc(xsize,ysize,zsize,bpp)
int xsize, ysize, zsize, bpp;
{
    TEXTURE *tm;
    unsigned char *buf;
    int y;

    tm = (TEXTURE *)malloc(sizeof(TEXTURE));
    tm->xsize = xsize;
    tm->ysize = ysize;
    tm->zsize = zsize;
    tm->bpp = bpp;
    tm->data = (unsigned char **)malloc(sizeof(char *)*ysize);
    buf = (unsigned char *)malloc(tm->bpp*tm->xsize*tm->ysize*tm->zsize);
    for(y=0; y<ysize; y++) {
	tm->data[y] = buf;
	buf += tm->bpp*tm->xsize*tm->zsize;
    }
    return tm;
}

TEXTURE *tmopen(name)
char *name;
{
    TEXTURE *tm;
    IMAGE *image;
    int y, z, bpp;;

    image = iopen(name,"r");
    if(!image) 
	return 0;
    if(BPP(image->type) == 1)
	bpp = 1;
    else
	bpp = 2;
    tm = tmalloc(image->xsize,image->ysize,image->zsize,bpp);
    for(y=0; y<image->ysize; y++) {
	for(z=0; z<image->zsize; z++) {
	    getrow(image,rowbuf,y,z);
	    copytotm(rowbuf,tm,y,z);
	}
    }
    iclose(image);
    return tm;
}

copytotm(rowbuf,tm,y,z)
short *rowbuf;
TEXTURE *tm;
int y, z;
{
    int n, zsize;
    unsigned char *cdata;
    unsigned short *sdata;

    zsize = tm->zsize;
    n = tm->xsize;
    if(tm->bpp == 1) {
	cdata = tm->data[y]+z;
	while(n--) {
	    *cdata = *rowbuf++;
	    cdata += zsize;
	}
    } else {
	sdata = ((unsigned short *)tm->data[y])+z;
	while(n--) {
	    *sdata = *rowbuf++;
	    sdata += zsize;
	}
    }
}

tmfree(tm)
TEXTURE *tm;
{
    if(tm) {
	free(tm->data[0]);
	free(tm->data);
	free(tm);
    }
}

tmclose(tm)
TEXTURE *tm;
{
    tmfree(tm);
}

tmsample(tm,v,c)
TEXTURE *tm;
vect *v, *c;
{
    int xpos;
    int ypos;

    xpos = onewrap(v->x)*(tm->xsize);
    if(xpos == tm->xsize)
	xpos = tm->xsize-1;
    ypos = onewrap(v->y)*(tm->ysize);
    if(ypos == tm->ysize)
	ypos = tm->ysize-1;
    imgsample(tm,xpos,ypos,c);
}

imgsample(tm,xpos,ypos,c)
register TEXTURE *tm;
register int xpos, ypos;
vect *c;
{
    int zsize;
    unsigned char *cdata;
    unsigned short *sdata;
    float val;

    zsize = tm->zsize;
    if(tm->bpp == 1) {
	cdata = tm->data[ypos]+xpos*zsize;
	switch(tm->zsize) {
	    case 1:
		c->x = c->y = c->z = *cdata/255.0;
		break;
	    case 2:
		c->x = *cdata++/255.0;
		c->y = c->z = *cdata/255.0;
		break;
	    case 3:
		c->x = *cdata++/255.0;
		c->y = *cdata++/255.0;
		c->z = *cdata/255.0;
		break;
	    case 4:
		c->x = *cdata++/255.0;
		c->y = *cdata++/255.0;
		c->z = *cdata++/255.0;
		c->w = *cdata/255.0;
		break;
	}
    } else {
	sdata = ((unsigned short *)tm->data[ypos])+xpos*zsize;
	switch(tm->zsize) {
	    case 1:
		c->x = c->y = c->z = *sdata/255.0;
		break;
	    case 2:
		c->x = *sdata++/255.0;
		c->y = c->z = *sdata/255.0;
		break;
	    case 3:
		c->x = *sdata++/255.0;
		c->y = *sdata++/255.0;
		c->z = *sdata/255.0;
		break;
	    case 4:
		c->x = *sdata++/255.0;
		c->y = *sdata++/255.0;
		c->z = *sdata++/255.0;
		c->w = *sdata/255.0;
		break;
	}
    }
}

#define XBIG	0
#define YBIG	1
#define ZBIG	2

static float deltax[6] = {
    0.0, 1.0, 2.0,
    0.0, 1.0, 2.0,
};

static float deltay[6] = {
    0.0, 0.0, 0.0,
    1.0, 1.0, 1.0,
};

envsample(tm,v,c)
TEXTURE *tm;
vect *v, *c;
{
    vect t;
    int xpos, ypos;

    vecttoenv(v,&t);
    xpos = t.x*tm->xsize;
    if(xpos >= tm->xsize)
	xpos = tm->xsize-1;
    ypos = t.y*tm->ysize;
    if(ypos >= tm->ysize)
	ypos = tm->ysize-1;
    imgsample(tm,xpos,ypos,c);
}

vecttoenv(v,e)
vect *v, *e;
{
    int code, seg;
    float tx, ty;
    float xabs, yabs, zabs;

    xabs = 2.001*ABS(v->x);
    yabs = 2.001*ABS(v->y);
    zabs = 2.001*ABS(v->z);
    if(xabs>yabs) {
	if(xabs>zabs) 
	    code = XBIG;
	else 
	    code = ZBIG;
    } else {
	if(yabs>zabs) 
	    code = YBIG;
	else 
	    code = ZBIG;
    }
    switch(code) {
	case XBIG:
	    if(xabs<0.001) {
		printf("vecttoenv\n");
		vprint(v);
	    }
	    tx = 0.5+(v->y/xabs);
	    ty = 0.5+(v->z/xabs);
	    if(v->x>0.0) 
		seg = 3;
	    else 
		seg = 0;
	    break;
	case YBIG:
	    if(yabs<0.001) {
		printf("vecttoenv\n");
		vprint(v);
	    }
	    tx = 0.5+(v->z/yabs);
	    ty = 0.5+(v->x/yabs);
	    if(v->y>0.0) 
		seg = 4;
	    else 
		seg = 1;
	    break;
	case ZBIG:
	    if(zabs<0.001) {
		printf("vecttoenv\n");
		vprint(v);
	    }
	    tx = 0.5+(v->x/zabs);
	    ty = 0.5+(v->y/zabs);
	    if(v->z>0.0) 
		seg = 5;
	    else 
		seg = 2;
	    break;
    }
    tx += deltax[seg];
    ty += deltay[seg];
    e->x = tx/3.0;
    e->y = ty/2.0;
    e->z = 0.0;
}

envtovect(e,v)
vect *e, *v;
{
    int seg;
    float tx, ty;

    tx = 3.0*e->x;
    ty = 2.0*e->y;
    seg = 0;
    if(ty>1.0) {
	seg = 3;
	ty -= 1.0;
    } else
	seg = 0;
    if(tx>2.0) {
	seg += 2;
	tx -= 2.0;
    } else if(tx>1.0) {
	seg += 1;
	tx -= 1.0;
    }
    tx -= 0.5;
    ty -= 0.5;
    switch(seg) {
	case 0:
	    v->x = -0.5;
	    v->y = tx;
	    v->z = ty;
	    break;
	case 1:
	    v->y = -0.5;
	    v->z = tx;
	    v->x = ty;
	    break;
	case 2:
	    v->z = -0.5;
	    v->x = tx;
	    v->y = ty;
	    break;
	case 3:
	    v->x = 0.5;
	    v->y = tx;
	    v->z = ty;
	    break;
	case 4:
	    v->y = 0.5;
	    v->z = tx;
	    v->x = ty;
	    break;
	case 5:
	    v->z = 0.5;
	    v->x = tx;
	    v->y = ty;
	    break;
    }
    vnormal(v);
}

spheretovect(s,v)
vect *s, *v;
{
    float mag, mag2, d;
    
    mag2 = s->x*s->x+s->y*s->y;
    if(mag2>=1.0) {
	v->x = 0.0;
	v->y = 0.0;
	v->z = 1.0;
	return 0;
    } else {
	d = sqrt(1.0-mag2);
	v->x = s->x;
	v->y = s->y;
	v->z = d;
	return 1;
    }
}

/*
 *	Signals 
 *
 */
float wave(pos)
vect *pos;
{
    float dist;

    dist = sqrt(pos->x*pos->x+pos->y*pos->y+pos->z*pos->z);
    dist = onewrap(dist); 
    return sin(DEGTORAD(dist*360.0));
}

float noisefunc(v)
vect *v;
{
    float x, y, z;

    x = v->x;
    y = v->y;
    z = v->z;
    return fnoise3(x,y,z);
}

#define NWAVES	10

float ocean(pos)
vect *pos;
{
    static int firsted;
    static vect origin[NWAVES];
    int i;
    float sum;
    vect p;

    if(!firsted) {
	for(i=0; i<NWAVES; i++) {
	    origin[i].x = 30.0*(frand()-0.5);
	    origin[i].y = 30.0*(frand()-0.5);
	    origin[i].z  = 0.0;
	}
	firsted++;
    }
    sum = 0.0;
    for(i=0; i<NWAVES; i++) {
	vadd(pos,origin+i,&p);
	sum += wave(&p);
    }
    return sum/NWAVES;
}

float turbulence(v,n)
vect *v;
int n;
{
    int i;
    float total, val, mag, mult;
    vect p;

    p = *v;
    vscale(&p,4.0);
    total = 0;
    mag = 0.0;
    mult = 1.0;
    for(i=0; i<n; i++) {
	val = mult*noisefunc(&p);
	total += ABS(val);
	mag += mult;
	mult /= 2.0;
	vscale(&p,2.0);
    }
    total /= mag;
    return total;
}


/*
 * 	Some textures 
 *
 */
texchecks(v,c)
vect *v, *c;
{
    vect vw;
    int val;
    
    vcubewrap(v,&vw);
    val = 1;
    if(vw.x>0.5)
	val = -val;
    if(vw.y>0.5)
	val = -val;
    if(vw.z>0.5)
	val = -val;
    if(val >0) {
	c->x = 1.0;
	c->y = 1.0;
	c->z = 1.0;
    } else {
	c->x = 0.0;
	c->y = 0.0;
	c->z = 0.0;
    }
}

texrampx(v,c)
vect *v, *c;
{
    float xpos;
    int val;
    
    xpos = onewrap(v->x);
    c->x = xpos;
    c->y = xpos;
    c->z = xpos;
}

/*
 *	noise -
 *		Fixed point implementation of Noise, in the C language
 *	from Ken Perlin.
 *
 */
#define SIZE	64

#define NP	(12)
#define N	(1<<NP)

#define BP	(8)
#define B	(1<<BP)

static s_curve[N];
static p[B+B+2];
static g[B+B+2][3];

#define setup(x,b0,b1,r0,r1,s) 			\
	x = x + (1<<(NP+BP));			\
	b0 = (x>>NP)&(B-1);			\
	b1 = (b0+1) &(B-1);			\
	r0 = x & (N-1);				\
	r1 = r0 - N;				\
	s = s_curve[r0];			
	
#define at(b,rx,ry,rz) 	(q = g[b],(rx*q[0]+ry*q[1]+rz*q[2])>>NP)

#define lerp(t,a,b)	(a+((t*(b-a))>>NP))

static int firsted = 0;

noise3(x,y,z)
int x, y, z;
{
	int bx0, bx1, by0, by1, bz0, bz1;
	int rx0, rx1, ry0, ry1, rz0, rz1;
	int b00, b10, b01, b11;
	int t000, t100, t010, t110;
	int t001, t101, t011, t111;
	int t00, t10, t01, t11;
	int t0, t1;
	register int sx, sy, sz;
	register int *q;

	if(!firsted) {
		init_noise3();
		firsted = 1;
	}

	setup(x,bx0,bx1,rx0,rx1,sx);
	setup(y,by0,by1,ry0,ry1,sy);
	setup(z,bz0,bz1,rz0,rz1,sz);

	b00 = p[p[bx0]+by0];
	b10 = p[p[bx1]+by0];
	b01 = p[p[bx0]+by1];
	b11 = p[p[bx1]+by1];

	t000 = at(b00+bz0,rx0,ry0,rz0);
	t100 = at(b10+bz0,rx1,ry0,rz0);
	t010 = at(b01+bz0,rx0,ry1,rz0);
	t110 = at(b11+bz0,rx1,ry1,rz0);
	t001 = at(b00+bz1,rx0,ry0,rz1);
	t101 = at(b10+bz1,rx1,ry0,rz1);
	t011 = at(b01+bz1,rx0,ry1,rz1);
	t111 = at(b11+bz1,rx1,ry1,rz1);

	t00 = lerp(sx,t000,t100);
	t10 = lerp(sx,t010,t110);
	t01 = lerp(sx,t001,t101);
	t11 = lerp(sx,t011,t111);

	t0 = lerp(sy,t00,t10);
	t1 = lerp(sy,t01,t11);

	return lerp(sz,t0,t1);
}

/*
 *	fnoise - 
 *		Return a value between -1.0 and +1.0
 *
 */
float fnoise3(x,y,z)
float x, y, z; 
{
    int ix, iy, iz;

    ix = N*x;
    iy = N*y;
    iz = N*z;
    return noise3(ix,iy,iz)/128.0;
}

static init_noise3()
{
	int i, j, k;
	double t;

	for(i=0; i<N; i++) {
		t = (double)i/N;
		if(i>N/2) t = 1.0-t;
		t = 4*t*t*t;
		if(i>N/2) t = 1.0-t;
		s_curve[i] = N*t;
	}
	for(i=0; i<B; i++) {
	    	p[i] = i;
		for(j=0; j<3; j++) {
			g[i][j] = (random()%(B+B))-B;
		}
		normalize(g[i]);
	}
	while(--i) {
		k = p[i];
		p[i] = p[j=random()%B];
		p[j] = k;
	}
	for(i=0; i<B+2; i++) {
		p[B+i] = p[i];
		for(j=0; j<3; j++)
			g[B+i][j] = g[i][j];
	}
}

static normalize(v)
int v[3];
{
	double s;

	s = sqrt((double)(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]));
	v[0] = (B*v[0])/s;
	v[1] = (B*v[1])/s;
	v[2] = (B*v[2])/s;
}

tmavg(tm,avg)
TEXTURE *tm;
vect *avg;
{
    int xsize, ysize;
    int x, y;
    vect c, acc;

    xsize = tm->xsize;
    ysize = tm->ysize;
    vzero(avg);
    for(y=0; y<ysize; y++) {
	vzero(&acc);
	for(x=0; x<xsize; x++) {
	    imgsample(tm,x,y,&c);
	    vadd(&c,&acc,&acc);
	}
	vscale(&acc,1.0/xsize);
	vadd(&acc,avg,avg);
    }
    vscale(avg,1.0/ysize);
}
