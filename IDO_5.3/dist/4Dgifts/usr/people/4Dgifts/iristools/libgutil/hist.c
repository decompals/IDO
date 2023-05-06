/*
 *	hist - 
 *		Support for histogram creation and display.
 *
 *				Paul Haeberli - 1988
 */
#include "hist.h"
#include "gl.h"

histogram *newhist(min,max,nbuckets)
int min, max;
int nbuckets;
{
    histogram *hist;
    int i, *ptr;

    hist = (histogram *)malloc(sizeof(histogram));
    hist->nbuckets = nbuckets;
    hist->bucket = (int *)malloc(nbuckets*sizeof(int));
    hist->min = min;
    hist->max = max;
    clearhist(hist);
    return hist;
}

freehist(hist)
histogram *hist;
{
    if(hist) {
        free(hist->bucket);
        free(hist);
    }
}

clearhist(hist)
register histogram *hist;
{
    register int i, nbuckets, *ptr;

    ptr = hist->bucket;
    nbuckets = hist->nbuckets;
    for (i=0; i<nbuckets; i++)
	*ptr++ = 0;
}
 
addtohist(hist,sptr,n)
histogram *hist;
register unsigned short *sptr;
register int n;
{
    register unsigned int index, nbuckets;
    register int *bucket;

    nbuckets = hist->nbuckets;
    bucket = hist->bucket;
    while (n--) {
	index = *sptr++;
	if(index>=nbuckets) 
	    index = nbuckets-1;
	bucket[index]++;
    }
}

showhist(hist)
register histogram *hist;
{
    int nbuckets;
    int i, bmax, j, total, *ptr;

    nbuckets = hist->nbuckets;
    total = bmax = 0;
    ptr = hist->bucket;
    for (i=0; i<nbuckets; i++) {
	total += *ptr;
	if(*ptr>bmax)
	    bmax = *ptr;
	ptr++;
    }
    if(bmax==0) 
	total = bmax = 1;

    ortho2(0.0,(float)nbuckets,0.0,(float)bmax);
    grey(0.4);
    clear();

    grey(0.5);
    ptr = hist->bucket;
    for (i=0; i<nbuckets; i++) 
	rectf(i+0.1,0.0,i+0.9,5.0*(float)(*ptr++));

    for (i=0; i<nbuckets; i+=10)  {
	if((i%100) == 0)
	    grey(0.0);
	else
	    grey(0.2);
	move2i(i,0);
	draw2i(i,bmax);
    }

    grey(0.8);
    ptr = hist->bucket;
    for (i=0; i<nbuckets; i++) 
	rectf(i+0.1,0.0,i+0.9,(float)(*ptr++));

    rgb(1.0,0.0,0.0);
    move2i(hist->min,0);
    draw2i(hist->min,bmax/40);
    move2i(hist->max,0);
    draw2i(hist->max,bmax/40);

    ortho2(0.0,(float)nbuckets,0.0,(float)total);
    grey(0.0);
    move2i(0,0);
    j = 0;
    ptr = hist->bucket;
    for (i=0; i<nbuckets; i++) {
	j += *ptr++;
	draw2((float)i,(float)j);
    }
}

histeqtable(hist,tab)
histogram *hist;
short *tab;
{
    int i, sum, shade, nbuckets; 
    register int *bucket; 
    float maxshade;

    nbuckets = hist->nbuckets;
    bucket = hist->bucket;
    sum = 0;
    for(i=0; i<nbuckets; i++)
	sum += bucket[i];
    if(sum == 0) 
	sum = 1;
    maxshade = 255.0;
    shade = 0;
    for(i=0; i<nbuckets; i++) {
	tab[i] = (shade*255.0)/sum;
	shade += bucket[i];
    }
}
