/*
 *	btree - 
 *		Display an image using a 2-D binary-tree representation.
 *
 *				Paul Haeberli - 1990
 *
 */
#include "gl.h"
#include "image.h"
#include "texture.h"

TEXTURE *tm;

long depth = 8;


mybitrev(val,n)
unsigned int val, n;
{
    val = ((val>>16)&0x0000ffff)|((val<<16)&0xffff0000);
    val = ((val>>8 )&0x00ff00ff)|((val<<8 )&0xff00ff00);
    val = ((val>>4 )&0x0f0f0f0f)|((val<<4 )&0xf0f0f0f0);
    val = ((val>>2 )&0x33333333)|((val<<2 )&0xcccccccc);
    val = ((val>>1 )&0x55555555)|((val<<1 )&0xaaaaaaaa);
    val = val>>(32-n);
    return val;
}

oddbits(val)
unsigned int val;
{
    int odd;

    odd = 0;
    odd |= val&0x01;
    val = val>>1;
    odd |= val&0x02;
    val = val>>1;
    odd |= val&0x04;
    val = val>>1;
    odd |= val&0x08;
    val = val>>1;
    odd |= val&0x10;
    val = val>>1;
    odd |= val&0x20;
    val = val>>1;
    odd |= val&0x40;
    val = val>>1;
    odd |= val&0x80;
    val = val>>1;
    odd |= val&0x100;
    val = val>>1;
    odd |= val&0x200;
    val = val>>1;
    odd |= val&0x400;
    return odd;
}

drawit()
{
    int bits;

    reshapeviewport();
    RGBcolor(128,128,128);
    clear();
    ortho2(0.0,1.0,0.0,1.0);
    for(bits=0; bits<depth; bits++) {
    	doit(bits);
	sleep(1);
    }
}

doit(bits)
int bits;
{
    int i, x, y, n, nsamp;
    int pos;
    vect p, c;
    float cv[3];
    float del;

    n = 1<<(bits);
    del = 0.50/n;
    nsamp = n*n;
    while(nsamp--) {
	pos = mybitrev(nsamp,bits+bits);
	x = oddbits(pos);
	y = oddbits(pos>>1);
	p.x = (0.5+x)/n;
	p.y = (0.5+y)/n;
	tmsample(tm,&p,&c);
	cv[0]=c.x;  cv[1]=c.y;  cv[2]=c.z;
	c3f(cv);
	pushmatrix();
	translate(p.x,p.y,0.0);
	rectf(-del,-del,del,del);
	popmatrix();
    }
}

main(argc,argv)
int argc;
char **argv;
{
    int xsize, ysize;
    int i;

    if ( argc<2 ) {
	fprintf(stderr,"usage: btree inimage [depth]\n");
	exit(1);
    }

    if (argc == 3) 
	depth = atoi(argv[2]);

    sizeofimage(argv[1],&xsize,&ysize);
    keepaspect(xsize,ysize);
    winopen("btree");
    RGBmode();
    gconfig();
    RGBcolor(128,128,128);
    clear();
    subpixel(1);
    tm = tmopen(argv[1]);
    drawfunc(drawit);
}
