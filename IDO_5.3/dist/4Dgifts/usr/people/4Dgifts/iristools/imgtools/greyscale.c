/*
 *	greyscale -
 *		Generate a variety of different patterns.
 *
 *				Paul Haeberli - 1986
 */
#include "stdio.h"
#include "image.h"
#include "math.h"

float flerp();

#define XRAMP		0
#define YRAMP		1
#define CONE		2
#define WHITE		3
#define STRIPES		4
#define RANDDOTS	5
#define CHECKS		6
#define GRID		7
#define GAMMATEST	8
#define GAUSSX		9
#define GAUSSY		10
#define CONTRAST	11
#define BLACKIMG	12
#define WHITEIMG	13
#define RANDOM		14
#define CIRCLE		15
#define ROLF		16
#define LINETEST	17
#define BAYER		18
#define DIRCONE		19
#define PRINTTEST	20
#define EXPRAMP		21
#define SHARPTHRESH	22
#define LIGHT		23
#define KERN242		24

#define DEGTORAD(d)	( (d)*(3.1415926535/180.0) )

short buf[8192];
short wbuf[8192];
short bbuf[8192];

short kern242[3][3] = {
    64, 128, 64,
    128, 255, 128,
    64, 128, 64,
};

short dithmat[4][4] = {
	0, 8, 2, 10,
	12, 4, 14, 6,
	3, 11, 1, 9,
	15, 7, 13, 5,
};

short sharpthresh[8][8] = {
57,	63,	41,	49,	55,	61, 	44,	51,
35,	55, 	21,	29,	33,	53,	24,	31,
27,	47, 	9,	13,	25,	45,	11,	15,
19,	39, 	1,	5,	17,	37,	3,	7,
40,	48,	54,	60,	42,	50,	56,	62,
20,	28,	32,	52,	22,	30,	34,	54,
8,	12,	24,	44,	10,	14,	26,	46,
0,	4,	16,	36,	2,	6,	18,	38,
};

float quadratic(x)	/* 3rd order (quadratic) b-spline */
float x;
{
    double t;

    if (x<-1.5) return 0.;
    if (x<-0.5) {
	t = x+1.5; 
	return 0.5*t*t;
    }
    if (x<0.5) return 0.75-x*x;
    if (x<1.5) {
	t = x-1.5; 
	return 0.5*t*t;
    }
    return 0.0;
}

float gauss(f)
float f;
{
    float val; 
    val = quadratic(3.0*(f-0.5))/0.75;
    return val;
}

float rolffunc(x)
float x;
{
    float y;

    if (x < -1.5)  
	y = 0.0;
    else if ((x >= -1.5) && (x < -.5))  
	y = 0.5 * (x + 1.5) * (x + 1.5);
    else if ((x >= -0.5) && (x < 0.5))  
	y = 0.5 + (x + 0.5) - (x + 0.5) * (x + 0.5);
    else if ((x >= 0.5) && (x < 1.5))  
	y = 0.5 - (x - 0.5) + 0.5 * (x - 0.5) * (x - 0.5);
    else  
	y = 0.0;
    return y;
}

float rolf(x)
float x;
{
    return (4.0/3.0)*rolffunc(x*3.0-1.5);
}

double logbase(n,b)
double n, b;
{
   return log10(n)/log10(b);
}

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *oimage;
    int x, y, pattern;
    float dx, dy, d;
    float fx, fy;
    int val;
    int xsize, ysize;
    float f, l, max;

    if(argc<5) {
	fprintf(stderr,"usage: greyscale outimage xsize ysize patterno\n");
	exit(1);
    }
    pattern = atoi(argv[4]);
    if(pattern == PRINTTEST) {
	xsize = 32;
        ysize = 2;
    } else {
        xsize = atoi(argv[2]);
        ysize = atoi(argv[3]);
    }
    oimage = iopen(argv[1],"w",RLE(1),2,xsize,ysize);
    switch(pattern) {
	case XRAMP:
	    for(x=0; x<xsize; x++) 
		buf[x] = (255*x)/(xsize-1);
	    for(y=0; y<ysize; y++)
	        putrow(oimage,buf,y,0);
	    break;
	case YRAMP:
	    for(y=0; y<ysize; y++)
		for(x=0; x<xsize; x++) 
		    putpix(oimage,(255*y)/(ysize-1));
	    break;
	case CONE:
	    for(y=0; y<ysize; y++)
		for(x=0; x<xsize; x++) {
		    dx = (xsize/2)-x;
		    dy = (ysize/2)-y;
		    d = sqrt(dx*dx+dy*dy);
		    putpix(oimage,(int)(512*d/(1.5*xsize)));
		}
	    break;
	case WHITE:
	    for(x=0; x<xsize; x++) 
		buf[x] = 255;
	    for(y=0; y<ysize; y++)
	        putrow(oimage,buf,y,0);
	    break;
	case STRIPES:
	    for(x=0; x<xsize; x++) 
		wbuf[x] = 255;
	    for(x=0; x<xsize; x++) 
		bbuf[x] = 0;
	    for(y=0; y<ysize; y++) {
		if((y%2) < 1)
		    putrow(oimage,bbuf,y,0);
		else
		    putrow(oimage,wbuf,y,0);
	    }
	    break;
	case RANDDOTS:
	    for(y=0; y<ysize; y++) {
		for(x=0; x<xsize; x++) {
		    if((random() % 101) == 35) 
			putpix(oimage,128+(random()%128));
		    else
			putpix(oimage,0);
		}
	    }
	    break;
	case CHECKS:
	    for(y=0; y<ysize; y++) {
		for(x=0; x<xsize; x++) {
		    if((x+y) % 2)
			putpix(oimage,255);
		    else
			putpix(oimage,0);
		}
	    }
	    break;
	case GRID:
	    for(y=0; y<ysize; y++) {
		if(y%10 == 0) {
		    for(x=0; x<xsize; x++) 
			putpix(oimage,0);
		} else {
		    for(x=0; x<xsize; x++) {
			if((x%10) == 0)
			    putpix(oimage,0);
			else
			    putpix(oimage,255);
		    }
		}
	    }
	    break;
	case GAMMATEST:
	    for(y=0; y<ysize; y++) {
		if( (y>>3)&1 ) {
		    for(x=0; x<xsize; x++) 
			putpix(oimage,127);
		} else {
		    for(x=0; x<xsize; x++) 
			putpix(oimage,254*(((x/2)+(y/2))&1));
		}
	    }
	    break;
	case GAUSSX:
	    for(x=0; x<xsize; x++) 
		buf[x] = 255*gauss(x/(xsize-1.0));
	    for(y=0; y<ysize; y++) 
		putrow(oimage,buf,y,0);
	    break;
	case GAUSSY:
	    for(y=0; y<ysize; y++) {
		val = 255*gauss(y/(ysize-1.0));
		for(x=0; x<xsize; x++) 
		    buf[x] = val;
		putrow(oimage,buf,y,0);
	    }
	    break;
	case CONTRAST:
	    for(y=0; y<ysize; y++) {
		for(x=0; x<xsize; x++) {
		    if((x%2) == 0) 
			putpix(oimage,64);
		    else
		        putpix(oimage,64+((y*(255-64))/(ysize-1)));
	      	}
	    }
	    break;
	case BLACKIMG:
	    for(x=0; x<xsize; x++) 
		buf[x] = 0;
	    for(y=0; y<ysize; y++) 
		putrow(oimage,buf,y,0);
	    break;
	case WHITEIMG:
	    for(x=0; x<xsize; x++) 
		buf[x] = 255;
	    for(y=0; y<ysize; y++) 
		putrow(oimage,buf,y,0);
	    break;
	case RANDOM:
	    for(y=0; y<ysize; y++) 
		for(x=0; x<xsize; x++) 
		    putpix(oimage,random()&0xff);
	    break;
	case CIRCLE:
	    for(y=0; y<ysize; y++)
		for(x=0; x<xsize; x++) {
		    dx = ((x+0.5)/xsize)-0.5;
		    dy = ((y+0.5)/ysize)-0.5;
		    d = sqrt(dx*dx+dy*dy);
		    if(d>0.5)
			putpix(oimage,0);
		    else
			putpix(oimage,255);
		}
	    break;
	case ROLF:
	    for(x=0; x<xsize; x++) 
		buf[x] = 255*rolf(x/(xsize-1.0));
	    for(y=0; y<ysize; y++)
		putrow(oimage,buf,y,0);
	    break;
	case LINETEST:
	    for(y=0; y<ysize; y++) {
		fy = y/(ysize-1.0);
	        for(x=0; x<xsize; x++) {
		    val = 127*(1.0+sin(DEGTORAD(60.0*(1.0+fy*2.0)*x)));
		    putpix(oimage,val);
		}
	    }
	    break;
	case BAYER:
	    for(y=0; y<ysize; y++) {
	        for(x=0; x<xsize; x++) 
		    putpix(oimage,16*dithmat[x&3][y&3]);
	    }
	    break;
	case DIRCONE:
	    for(y=0; y<ysize; y++) {
	        for(x=0; x<xsize; x++) {
		    dx = ((x+0.5)/xsize)-0.5;
		    dy = ((y+0.5)/ysize)-0.5;
		    val = 255*atan2(dx,dy)/(2.0*3.1415926535);
		    val = 255-val;
		    val += 64;
		    if(val>255)
			val -= 256;
		    putpix(oimage,val);
		}
	    }
	    break;
	case PRINTTEST:
	    for(x=0; x<31; x++) {
		putpix(oimage,255-2*x);
	    }
	    putpix(oimage,128);
	    for(x=0; x<31; x++) {
		putpix(oimage,2*x);
	}
	putpix(oimage,128);
	    break;
	case EXPRAMP:
	    max = logbase(255.0,2.0);
	    for(x=0; x<xsize; x++) {
		f = max*flerp(0.2,1.0,x/(xsize-1.0));
		f = pow(2.0,f);
		buf[x] = f+0.5;
		if(x==0 || x==xsize-1) 
		    fprintf(stderr,"i %d val %f\n",x,f);
	    }
	    for(y=0; y<ysize; y++) 
		putrow(oimage,buf,y,0);
	case SHARPTHRESH:
	    for(y=0; y<ysize; y++) {
	        for(x=0; x<xsize; x++) 
		    putpix(oimage,4*sharpthresh[x&7][y&7]);
	    }
	    break;
	case LIGHT:
	    for(y=0; y<ysize; y++) {
		fy = 2.0*((y/(ysize-1.0))-0.5);
		fy = fy*fy;
		d = 1.0;
		d = d*d;
	        for(x=0; x<xsize; x++) {
		    fx = 2.0*((x/(xsize-1.0))-0.5);
		    fx = fx*fx;
		    val = (d*255)/(fx+fy+d);
		    putpix(oimage,val);
		}
	    }
	    break;
	case KERN242:
	    for(y=0; y<ysize; y++) {
		for(x=0; x<xsize; x++) {
		    putpix(oimage,kern242[y%3][x%3]);
		}
	    }
    }
    iclose(oimage);
    exit(0);
}
