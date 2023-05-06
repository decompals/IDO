/*
 *	perhist - 
 *		Determine the percentage histogram of an image file.
 *
 *			Paul Haeberli and Pat Hanrahan - 1989
 *
 */
#include "math.h"
#include "image.h"
#include "hist.h"

unsigned short rowbuf[8192];
int xsize, ysize;
int xorg, yorg;

main(argc,argv)
int argc;
char **argv;
{
    register IMAGE *image;
    register unsigned int i, index;
    short val;
    int x, y, z;
    int totpixels;
    histogram *hists[3];
    int nhists;
    float minpercent, maxpercent;
    int min, rmin, gmin, bmin;
    int max, rmax, gmax, bmax;
    int put3; 
    i=0;

    if( argc<2 ) {
	fprintf(stderr,"usage: perhist inimage [minpercent maxpercent] [-3]\n");
	exit(0);
    } 
    if( (image=iopen(argv[1],"r")) == NULL ) {
	fprintf(stderr,"hist: can't open input file %s\n",argv[1]);
	exit(0);
    }
    if(argc == 3 | argc == 5)
	put3 = 1;
    else 
	put3 = 0;
    minpercent = 1.0;
    maxpercent = 99.0;
    if( argc == 4 ) {
	minpercent = atof(argv[2]);
	maxpercent = atof(argv[3]);
    }
    minpercent = minpercent/100.0;
    maxpercent = maxpercent/100.0;
    if(image->max>255) {
	fprintf(stderr,"hist: max can't exceed 255\n");
	exit(0);
    }
    if(image->zsize<3) {
	hists[0] = newhist(image->min,image->max,256);
	nhists = 1;
    } else {
	for(x=i; i<3; i++)
	    hists[i] = newhist(image->min,image->max,256);
	nhists = 3;
    }
    totpixels = image->zsize*image->ysize*image->xsize;
    for(y=0; y<image->ysize; y++) {
	if(image->zsize<3) {
	    getrow(image,rowbuf,y,0);
	    addtohist(hists[0],rowbuf,image->xsize);
	} else {
	    getrow(image,rowbuf,y,0);
	    addtohist(hists[0],rowbuf,image->xsize);
	    getrow(image,rowbuf,y,1);
	    addtohist(hists[1],rowbuf,image->xsize);
	    getrow(image,rowbuf,y,2);
	    addtohist(hists[2],rowbuf,image->xsize);
	}
    }
    if( image->zsize<3 ) {
	min = percentage( hists[0], minpercent );
	max = percentage( hists[0], maxpercent );
	max = maxhack(max);
	rmin = gmin = bmin = min;
	rmax = gmax = bmax = max;
    } else {
	rmin = percentage( hists[0], minpercent );
	rmax = percentage( hists[0], maxpercent );
	gmin = percentage( hists[1], minpercent );
	gmax = percentage( hists[1], maxpercent );
	bmin = percentage( hists[2], minpercent );
	bmax = percentage( hists[2], maxpercent );
	rmax = maxhack(rmax);
	gmax = maxhack(gmax);
	bmax = maxhack(bmax);
	min = rmin;
	if( gmin < min ) min = gmin;
	if( bmin < min ) min = bmin;
	max = rmax;
	if( gmax > max ) max = gmax;
	if( bmax > max ) max = bmax;
    }
    if(put3) 
	printf("%d %d %d %d %d %d\n",rmin,rmax,gmin,gmax,bmin,bmax);
    else
	printf("%d %d\n",min,max );
    exit(0);
}

int percentage( hist, percent )
histogram *hist;
float percent;
{
    int i;
    long sum;
    long tot;

    tot = 0;
    for( i=0; i<hist->nbuckets; i++ ) {
	tot += hist->bucket[i];
    }
    tot = percent * tot;

    sum = 0;
    for( i=0; i<hist->nbuckets; i++ ) {
	sum += hist->bucket[i];
	if( sum >= tot )
	    break;
    }
    return i;
}

maxhack(m)
int m;
{
    m = m*1.05;
    if(m>255)
	m = 255;
    return m;
}
