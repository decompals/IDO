/*
 *	abekas- 
 *		Support for digital video conversions.
 *
 * 		    		Paul Haeberli - 1993
 *	exports
 *
	void zoom720to640(ibuf,obuf)
	void zoom640to720(ibuf,obuf)
	void YUVdither(doit)
	void UYVYtoRGB(ap,rp,gp,bp)
	void RGBtoUYVY(rp,gp,bp,ap) 
 *
 */
#include "stdio.h"
#include "math.h"

#define NGROUPS		(720/9)
#define	XSIZE		(720)

static int dodither;

void zoom720to640(ibuf,obuf)
short *ibuf,  *obuf;
{
     int n;

     for(n=0; n<NGROUPS; n++) {
	 obuf[0] = ((8*ibuf[0])+(1*ibuf[1])+4)/9;
	 obuf[1] = ((7*ibuf[1])+(2*ibuf[2])+4)/9;
	 obuf[2] = ((6*ibuf[2])+(3*ibuf[3])+4)/9;
	 obuf[3] = ((5*ibuf[3])+(4*ibuf[4])+4)/9;
	 obuf[4] = ((4*ibuf[4])+(5*ibuf[5])+4)/9;
	 obuf[5] = ((3*ibuf[5])+(6*ibuf[6])+4)/9;
	 obuf[6] = ((2*ibuf[6])+(7*ibuf[7])+4)/9;
	 obuf[7] = ((1*ibuf[7])+(8*ibuf[8])+4)/9;
	 obuf += 8;
	 ibuf += 9;
     }
}

void zoom640to720(ibuf,obuf)
short *ibuf,  *obuf;
{
     int n;

     for(n=0; n<NGROUPS; n++) {
	 obuf[0] = ((8*ibuf[0])+(0*ibuf[0])+4)/8;
	 obuf[1] = ((7*ibuf[1])+(1*ibuf[0])+4)/8;
	 obuf[2] = ((6*ibuf[2])+(2*ibuf[1])+4)/8;
	 obuf[3] = ((5*ibuf[3])+(3*ibuf[2])+4)/8;
	 obuf[4] = ((4*ibuf[4])+(4*ibuf[3])+4)/8;
	 obuf[5] = ((3*ibuf[5])+(5*ibuf[4])+4)/8;
	 obuf[6] = ((2*ibuf[6])+(6*ibuf[5])+4)/8;
	 obuf[7] = ((1*ibuf[7])+(7*ibuf[6])+4)/8;
	 obuf[8] = ((0*ibuf[7])+(8*ibuf[7])+4)/8;
	 obuf += 9;
	 ibuf += 8;
     }
}

#define LIMIT(r,x) { 			\
    r = x;				\
    if(r >  0x00ffffff) r = 0x00ffffff; \
    if(r <= 0x00000000) r = 0; 		\
}

void YUVdither(doit)
int doit;
{
    dodither = doit;
}

/*
 * 	256*256*255/219 is 76309.04109589 this is "1" for y
 * 	256*256*255/223 is 74940.26905830 this is "1" for u and v
 *
 *	The inverse matrix is:
 *
 *	1.000000         0.000000         1.402581        
 *	1.000000        -0.344369        -0.714407       
 *	1.000000         1.773043        -0.000000       
 *
 */
void UYVYtoRGB(ap,rp,gp,bp)
unsigned char *ap;
short *rp, *gp, *bp;
{
    int j; 
    long y, u, v, y1, r, g, b;
    long qr, qg, qb;

    if(dodither) {
	qr = random()%0x4000;
	qg = random()%0x4000;
	qb = random()%0x4000;
    }
    for(j=0; j<(XSIZE/2); j++) {
	u = ap[0];
	u -= 128;
	y = ap[1];
	y -= 16;
	if(y<0) y = 0;

	v = ap[2];
	v -= 128;
	y1 = ap[3];
	y1 -= 16;
	if(y1<0) y1 = 0;

	ap+=4;

	y *= 76309;	
	r  =    y  +            105110*v;
	g  =    y  + -25807*u + -53538*v;
	b  =    y  + 132872*u;

  	if(dodither) {
	    LIMIT(qr,r+(qr & 0xffff));
	    LIMIT(qg,g+(qg & 0xffff));
	    LIMIT(qb,b+(qb & 0xffff));
 	} else {
	    LIMIT(qr,r);
	    LIMIT(qg,g);
	    LIMIT(qb,b);
	}

	*rp++ = qr>>16;
	*gp++ = qg>>16;
	*bp++ = qb>>16;

	y1 *= 76309;
	r  =    y1 +            105110*v;
	g  =    y1 + -25807*u + -53538*v;
	b  =    y1 + 132872*u;

  	if(dodither) {
	    LIMIT(qr,r+(qr & 0xffff));
	    LIMIT(qg,g+(qg & 0xffff));
	    LIMIT(qb,b+(qb & 0xffff));
	} else {
	    LIMIT(qr,r);
	    LIMIT(qg,g);
	    LIMIT(qb,b);
	}

	*rp++ = qr>>16;
	*gp++ = qg>>16;
	*bp++ = qb>>16;
    }
}

/*
 * 	256*256*219/255 is 56283.85882352 this is "1" for y
 * 	256*256*223/255 is 57311.87450980 this is "1" for u and v
 *
 *	The forward matrix is:
 *
 *	 0.2990        	 0.5870        	 0.1140
 *	-0.1686        	-0.3311        	 0.4997
 *	 0.4998        	-0.4185        	-0.0813
 *
 */
void RGBtoUYVY(rp,gp,bp,ap) 
unsigned short *rp, *gp, *bp;
unsigned char *ap;
{ 
    int i, r, g, b;
    long y1, y2, u, v, u1, u2, v1, v2;

    if(dodither) { 
	y1 = random()%0x4000;
	y2 = random()%0x4000;
    } else {
	y1 = 0;
	y2 = 0;
    }
    for(i = XSIZE/2; i>0; i--) { 

/* first pixel gives Y and 0.5 of chroma */ 
	r = *rp++;
	g = *gp++;
	b = *bp++;

	y1  =    16829*r +  33039*g +  6416*b;
	u1  =    -4831*r +  -9488*g + 14319*b;
	v1  =    14322*r + -11992*g + -2330*b;
	if(dodither) y1 += (y2&0xffff);

/* second pixel gives Y and 0.5 of chroma */ 
	r = *rp++;
	g = *gp++;
	b = *bp++;

	y2  =    16829*r +  33039*g +  6416*b;
	u2  =    -4831*r +  -9488*g + 14319*b;
	v2  =    14322*r + -11992*g + -2330*b;
	if(dodither) y2 += (y1&0xffff);

/* average the chroma */
	u = u1 + u2;
	v = v1 + v2;

/* round the chroma */
	u1 = (u+0x008000)>>16;
	v1 = (v+0x008000)>>16;

/* limit the chroma */
	if(u1<-112) u1 = -112;
	if(u1>111) u1 = 111;
	if(v1<-112) v1 = -112;
	if(v1>111) v1 = 111;

/* limit the lum */
   	if(y1>0x00dbffff) y1=0x00dbffff;
   	if(y2>0x00dbffff) y2=0x00dbffff;

/* save the results */
	ap[0] = u1 +128;
	ap[1] = (y1>>16) + 16;
	ap[2] = v1 +128;
	ap[3] = (y2>>16) + 16;
	ap += 4;
    }
} 
