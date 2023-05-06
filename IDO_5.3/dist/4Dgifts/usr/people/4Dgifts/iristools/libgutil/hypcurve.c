/*
 *	hypcurve -
 *		Create hyperbolic and power curves for correcting
 *	printers. 
 *
 *				Paul Haeberli - 1990
 */
#include "math.h"
#include "stdio.h"

float hypfunc(hypval,powval,x)
float hypval, powval, x;
{
    float b, y;

    if(x<0.00001)
	return 0.0;
    if(hypval<0.99999 || hypval>1.00001) {
	b = (1.0/hypval) - hypval;
	y = 1.0-(1.0/((x*b)+hypval) - hypval)/b;
    } else 
	y = x;
    return pow(y,powval);
}

float hypinvfunc(hypval,powval,y)
float hypval,powval,y;
{
    float b, x;

    if(y<0.00001)
	return 0.0;
    y = pow(y,1.0/powval);
    if(hypval<0.99999 || hypval>1.00001) {
	hypval = 1.0/hypval;
	b = (1.0/hypval) - hypval;
	x = 1.0-(1.0/((y*b)+hypval) - hypval)/b;
    } else 
	x = y;
    return x;
}

hypcurve(hypval,powval,tab)
float hypval, powval;
short tab[256];
{
    float x, y;
    int i;

    for(i=0; i<256; i++) {
	x = i/255.0;
	y = hypfunc(hypval,powval,x);
	tab[i] = (255*y)+0.5;
    }
}


hypinvcurve(hypval,powval,tab)
float hypval, powval;
short tab[256];
{
    float x, y;
    int i;

    for(i=0; i<256; i++) {
	x = i/255.0;
	y = hypinvfunc(hypval,powval,x);
	tab[i] = (255*y)+0.5;
    }
}

printtab(tab)
short tab[256];
{
    int i;

    fprintf(stderr,"cortab: \n");
    for(i=0; i<256; i++) {
	if((i%16) == 0)
	    fprintf(stderr,"\n");
	fprintf(stderr,"%03d ",tab[i]);
    }
    fprintf(stderr,"\n");
}
