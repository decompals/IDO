/*
 *	dotgen - 
 *		Generate a threshold image of halftone dots.
 *
 *				Paul Haeberli - 1988
 */
#include "vect.h"
#include "image.h"

#define PI		(3.1415926535)
#define DEGTORAD(x)	(((x)*PI)/180.0)

short buf[8192];
float scalex, scaley;

main(argc,argv)
int argc;
char **argv;
{
    int size, abs;
    int x, y;
    IMAGE *image;
    vect pos, c;

    if(argc<5) {
       fprintf(stderr,"usage: dotgen outimage size scalex scaley [-a]\n");
       exit(1);
    }
    if(argc>5) 
	abs = 1;
    else
	abs = 0;
    size = atoi(argv[2]);
    image = iopen(argv[1],"w",RLE(1),2,size,size,1);
    scalex = 2*atof(argv[3]);
    scaley = 2*atof(argv[4]);
    for(y=0; y<size; y++) {
	for(x=0; x<size; x++) {
	    pos.x = (float)x/size;
	    pos.y = (float)y/size;
	    pos.z = 0.0;
	    sampletex(&pos,&c,abs);
	    buf[x] = 255*c.x;
	}
	putrow(image,buf,y,0);
    }
    iclose(image);
    exit(0);
}

oldsampletex(p,c,abs)
vect *p, *c; 
int abs;
{
    float val;
    vect v;
    float temp;
    float x, y;

    vscale(p,3.1415926535);
    x = scalex*p->x + scaley*p->y;
    y = scalex*p->y - scaley*p->x;
    val = (sin(x)+sin(y))/2.0;
    if(abs) {
        if(val<0.0)
	    val = -val;
    } else
	val = (val+1.0)/2.0;
    c->x = val;
    c->y = val;
    c->z = val;
}

vect *makerot(angle)
float angle;
{
    float x, y;
    float s, c;
    vect *v;

    v = vnew();
    v->y = sin(angle*(3.1415926/180.0));
    v->x = cos(angle*(3.1415926/180.0));
    return v;
}

float vproject(v,r)
vect *v, *r;
{
    return r->x*v->x+r->y*v->y;
}

vect *rot0;
vect *rot90;
vect *rot120;
vect *rot240;

sampletex(p,c,abs)
vect *p, *c; 
int abs;
{
    float val;
    vect v;
    float temp;
    float v0, v1, v2;
    float x, y;

#ifdef THREEWAY
    if(!rot0) {
	rot0 = makerot(0.0);
	rot120 = makerot(120.0);
	rot240 = makerot(240.0);
    }
    vscale(p,PI);
    x = scalex*p->x + scaley*p->y;
    y = scalex*p->y - scaley*p->x;
    p->x = x;
    p->y = y;
    v0 = vproject(p,rot0);
    v1 = vproject(p,rot120);
    v2 = vproject(p,rot240);
    val = (sin(v0)+sin(v1)+sin(v2))/3.0;
    if(abs) {
        if(val<0.0)
	    val = -val;
    } else
	val = (val+1.0)/2.0;
    c->x = val;
    c->y = val;
    c->z = val;
#endif
    if(!rot0) {
	rot0 = makerot(0.0);
	rot90 = makerot(90.0);
    }
    vscale(p,PI);
    x = scalex*p->x + scaley*p->y;
    y = scalex*p->y - scaley*p->x;
    p->x = x;
    p->y = y;
    v0 = vproject(p,rot0);
    v1 = vproject(p,rot90);
    val = (sin(v0)+sin(v1))/2.0;
    if(abs) {
        if(val<0.0)
	    val = -val;
    } else
	val = (val+1.0)/2.0;
    c->x = val;
    c->y = val;
    c->z = val;
}
