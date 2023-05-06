/*
 *	lut - 
 *		Create and apply look up tables.
 *
 *			    Paul Haebeli - 1990
 */
#include "lut.h"

float frand();

lut *makelut(func,insteps,outsteps,stoclut)
float (*func)();
int insteps, outsteps, stoclut;
{
    lut *l;
    int i;
    float low, high, inspace;

    l = (lut *)malloc(sizeof(lut));
    l->insteps = insteps;
    l->outsteps = outsteps;
    l->stoclut = stoclut;
    if(stoclut) {
	l->flow = (float *)malloc(insteps*sizeof(float));
	l->fhigh = (float *)malloc(insteps*sizeof(float));
	inspace = insteps-1.0;
	for(i=0; i<insteps; i++) {
	    low = (i-0.5)/inspace;
	    high = (i+0.5)/inspace;
	    if(low<0.0)
		low = 0.0;
	    if(high>1.0)
		high = 1.0;
	    l->flow[i] = (func)(low);
	    l->fhigh[i] = (func)(high);
	}
    } else { 
	l->stab = (unsigned short *)malloc(insteps*sizeof(unsigned short));
	inspace = insteps-1.0;
	for(i=0; i<insteps; i++) 
	    l->stab[i] = (l->outsteps-1)*(func)(i/inspace)+0.5;
    }
    return l;
}

applylut(l,sptr,n)
lut *l;
unsigned short *sptr;
int n;
{
    float delta, val;
    float *fhigh, *flow;
    unsigned short *stab;
    float outspace;
    int ival;

    if(l->stoclut) {
	fhigh = l->fhigh;
	flow = l->flow;
	outspace = l->outsteps-1;
	while(n--) {
	    delta = fhigh[*sptr]-flow[*sptr];
	    val = outspace*(flow[*sptr]+frand()*delta);
	    ival = val;
	    if((val-ival)<frand()) 
		*sptr++ = ival;
	    else
		*sptr++ = ival+1;
	}
    } else {
	stab = l->stab;
	while(n--) {
	    *sptr = stab[*sptr];
	    sptr++;
	}
    }
}
