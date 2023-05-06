/*
 *	convolve - 
 *		Convolve one image with another.
 *
 *				Paul Haeberli - 1987
 */
#include "image.h"
#include "stdio.h"
#include "math.h"

#define MAXXSIZE	8192

#define CONVOLVE	0
#define KERNMAX     	1
#define KERNDELTA     	2
#define GRADIENT     	3
#define MEDIAN     	4
#define QUANT     	5
#define HISTEQ     	6

short obuf[MAXXSIZE];
short zapbuf[MAXXSIZE];
int kmax = 0;
int mode = CONVOLVE;
int ninside;
short *sortbuf;

short **rowptr;
short *tempptr;
short *kernel;
short *readkernel();

main(argc,argv)
int argc;
char **argv;
{
    IMAGE *iimage, *oimage, *kimage;
    register short *opixptr;
    register short *oipixptr;
    register int conv, val;
    register int x, y, z;
    register int kx, ky;
    register int maxx, sum;
    int xsize, ysize, zsize;
    int r, yout;
    int xoffset, yoffset;
    int kxsize, kysize;

/* open input and output images */
    if( argc<4 ) {
	fprintf(stderr,"usage: convolve inimage outimage kernelimage [-m -d -g -e -q -h]\n");
	exit(1);
    } 
    if(argc>4) {
	if(argv[4][0] != '-') {
	    fprintf(stderr,"convolve: bad option %s\n",argv[4]);
	    exit(1);
	}
	switch(argv[4][1]) {
	    case 'm':
		mode = KERNMAX;
		break;
	    case 'd':
		mode = KERNDELTA;
		break;
	    case 'g':
		mode = GRADIENT;
		break;
	    case 'e':
		mode = MEDIAN;
		break;
	    case 'q':
		mode = QUANT;
		break;
	    case 'h':
		mode = HISTEQ;
		break;
	    default:
		fprintf(stderr,"convolve: bad option %s\n",argv[4]);
		exit(1);
	}
    }
    iimage = iopen(argv[1],"r");
    if(!iimage) {
	fprintf(stderr,"convolve: can't open input file %s\n",argv[1]);
	exit(1);
    }
    xsize = iimage->xsize;
    ysize = iimage->ysize;
    zsize = iimage->zsize;
    oimage = iopen(argv[2],"w",RLE(1),iimage->dim,xsize,ysize,zsize);

/* read in the kernel image */
    kimage = iopen(argv[3],"r");
    if(!kimage) {
	fprintf(stderr,"convolve: can't open input file %s\n",argv[3]);
	exit(1);
    }
    kxsize = kimage->xsize;
    kysize = kimage->ysize;
    kernel = readkernel(kimage);
    rowptr = (short**)malloc(kysize*sizeof(short *));
    sum = 0;
    kmax = 0;
    ninside = 0;
    for(ky=0; ky<kysize; ky++) {
	for(kx=0; kx<kxsize; kx++) {
	    if((mode == KERNDELTA)||(mode == MEDIAN)||(mode == HISTEQ)) {
	        if(kernel[kxsize*ky+kx] >128) {
		    kernel[kxsize*ky+kx] = 1;
		    ninside++;
		} else
		    kernel[kxsize*ky+kx] = 0;
	    }
	    val = kernel[kxsize*ky+kx];
	    if(val>kmax)
		kmax = val;
	    sum += val;
	}
    }
    if(ninside>0) 
	sortbuf = (short *)malloc(ninside*sizeof(short));
    if(sum == 0) {
	fprintf(stderr,"convolve: whole kernel is zero\n");
	exit(1);
    }
    for(ky=0; ky<kysize; ky++) 
	rowptr[ky] = (short *)malloc(MAXXSIZE*sizeof(short));
    xoffset = kxsize/2;
    yoffset = kysize/2;

/* do the convolvtion */
    maxx = xsize-(kxsize-1);
    for(z=0; z<zsize; z++) {
	yout = 0;
	for(y=0; y<kysize-1; y++) {
	    mygetrow(iimage,rowptr[y],y,z);
	    if(y<yoffset)
		putrow(oimage,zapbuf,yout++,z);
	}
	for(; y<ysize; y++) {
	    mygetrow(iimage,rowptr[kysize-1],y,z);
	    opixptr = obuf+xoffset;
	    for(x=0; x<maxx; x++) 
		*opixptr++ = convolve(rowptr,kernel,kxsize,kysize,x,sum);
	    putrow(oimage,obuf,yout++,z);
	    tempptr = rowptr[0];
	    for(r=0; r<(kysize-1); r++) 
		rowptr[r] = rowptr[r+1];
	    rowptr[r] = tempptr;
	}
  	while(yout<ysize)
	    putrow(oimage,zapbuf,yout++,z);
    }
    iclose(oimage);
    exit(0);
}

short *readkernel(image)
IMAGE *image;
{
    int xsize, ysize;
    int y;
    short *buf, *sptr;;

    xsize = image->xsize;
    ysize = image->ysize;
    sptr = buf = (short *)malloc(xsize*ysize*sizeof(short));
    for(y=0; y<ysize; y++) {
	mygetrow(image,sptr,y,0);
	sptr += xsize;
    }
    return buf;
}

float fakeatan(a,b) 
float a, b;
{
    a = a/b;
    if(a>0.5773502) 
	return 0.666666 + (0.333333/(1.0-0.5773502))*(a-0.5773502);
    else
	return a*(0.666666/0.5773502);
}

#define PI	3.1415926535

float toslope(dx,dy)
float dx, dy;
{
    int flipx, flipy;
    float val;

    val = -atan2(dx,dy);
    if(val<0.0)
	val = val+(2*PI); 
    return (val/(2*PI));
}

scomp(a,b)
short *a, *b;
{
    if(*a<*b) 
	return -1;
    else if(*a==*b) 
	return 0;
    else
	return 1;
}

#define HISTSIZE	32
int hist[HISTSIZE];

convolve(rowptr,kernel,xsize,ysize,x,sum)
short **rowptr, *kernel;
int xsize, ysize;
register int  x, sum;
{
    register int conv, val;
    register int kx, ky;
    register int min, max, multmax;
    register short *ipixptr;
    register short *kpixptr;
    int v00, v01, v10, v11;
    float slope, didx, didy;
    int n, pix;
    int maxp, a, b, delta;

    switch(mode) {
	case CONVOLVE:
	    conv = 0;
	    if(xsize == 1) {
		kpixptr = kernel;
		for(ky=ysize; ky--;) {
		    ipixptr = (*rowptr++)+x;
		    conv += *kpixptr++ * *ipixptr;
		}
	    } else {
		for(ky=ysize; ky--;) {
		    ipixptr = (*rowptr++)+x;
		    kpixptr = kernel;
		    kernel += xsize;
		    for(kx=xsize; kx--;) 
			conv += *kpixptr++ * *ipixptr++;
		}
	    }
	    conv /= sum;
	    if(conv<0)
		conv = 0;
	    if(conv>255)
		conv = 255;
	    return conv;
	case KERNMAX:
	    conv = 0;
	    multmax = 255*kmax;
	    for(ky=ysize; ky--;) {
		ipixptr = (*rowptr++)+x;
		kpixptr = kernel;
		kernel += xsize;
		for(kx=xsize; kx--;) {
		    val = *kpixptr++ * *ipixptr++;
		    if(val > conv) {
			conv = val;
			if(conv == multmax)
			    break;
		    }
		}
		if(conv == multmax)
		    break;
	    }
	    conv = conv/kmax;
	    return conv;
	case KERNDELTA:
	    min = 255;
	    max = 0;
	    for(ky=ysize; ky--;) {
		ipixptr = (*rowptr++)+x;
		kpixptr = kernel;
		kernel += xsize;
		for(kx=xsize; kx--;) {
		    if(*kpixptr++) {
		        val = *ipixptr++;
		        if(val<min)
			    min = val;
		        if(val>max)
			    max = val;
		    } else 
		        ipixptr++;
		}
	    }
	    if(max>min)
	        return max-min;
	    else 
		return 0;
	case GRADIENT:
	    ipixptr = (*rowptr)+x;
	    v00 = ipixptr[0];
	    v01 = ipixptr[xsize-1];
	    rowptr += ysize-1;
	    ipixptr = (*rowptr)+x;
	    v10 = ipixptr[0];
	    v11 = ipixptr[xsize-1];
	    didx = (v01+v11)-(v00+v10);
	    didy = (v10+v11)-(v00+v01);
	    return 255*toslope(didx,didy);
	case MEDIAN:
	    n = 0;
	    for(ky=ysize; ky--;) {
		ipixptr = (*rowptr++)+x;
		kpixptr = kernel;
		kernel += xsize;
		for(kx=xsize; kx--;) {
		    if(*kpixptr++)
			sortbuf[n++] = *ipixptr++;
		    else 
		        ipixptr++;
		}
	    }
	    qsort(sortbuf,n,sizeof(short),scomp);
	    return sortbuf[n/2];
	case  QUANT:
	    ipixptr = *(rowptr+ysize/2);
	    pix = ipixptr[x+xsize/2];
	    for(kx=0; kx<HISTSIZE; kx++)
		hist[kx] = 0;
	    for(ky=ysize; ky--;) {
		ipixptr = (*rowptr++)+x;
		kpixptr = kernel;
		kernel += xsize;
		for(kx=xsize; kx--;) {
		    if(*kpixptr++)
			hist[(HISTSIZE-1)*(*ipixptr++)/255]++;
		    else 
		        ipixptr++;
		}
	    }
	    max = hist[0];
	    maxp = 0;
	    for(kx=1; kx<HISTSIZE; kx++) {
		if(hist[kx]>max) {
		    max = hist[kx];
		    maxp = kx;
		}
	    }
	    a = (maxp*255)/HISTSIZE;;
	    hist[maxp] = 0;

	    max = hist[HISTSIZE-1];
	    maxp = HISTSIZE-1;
	    for(kx=HISTSIZE-1; kx--;) {
		if(hist[kx]>max) {
		    max = hist[kx];
		    maxp = kx;
		}
	    }
	    b = (maxp*255)/HISTSIZE;;
	    if((a-pix)*(a-pix)<(b-pix)*(b-pix))
   		return a;
	    else 
   		return b;
	case HISTEQ:
	    n = 0;
	    ipixptr = *(rowptr+ysize/2);
	    pix = ipixptr[x+xsize/2];
	    for(ky=ysize; ky--;) {
		ipixptr = (*rowptr++)+x;
		kpixptr = kernel;
		kernel += xsize;
		for(kx=xsize; kx--;) {
		    if(*kpixptr++) {
			if(*ipixptr++ <= pix)
			    n++;
		    } else
		        ipixptr++;
		}
	    }
	    return (n*255)/ninside;
    }
}

mygetrow(image,buf,y,z)
IMAGE *image;
short *buf;
int y, z;
{
    while(y<0)
	y += image->ysize;
    y = y % image->ysize;
    getrow(image,buf,y,z);
}
