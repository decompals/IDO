/*
 * 	gamma - 
 *		Get or set the gamma value stored in 
 *              /etc/config/system.glGammaVal.
 *
 *				Paul Haeberli - 1984
 *
 */
#include "gl.h"
#include "stdio.h"
#include "math.h"


float getgamma();

main(argc, argv)
int argc;
char **argv;
{
    double val;

    if (argc>3) 
	fprintf(stderr,"usage: gamma gammavalue\n");
    else if (argc>1) {
        val = atof(argv[1]);
	if( (val > 0.) && (val < 20.) ) {
        foreground();
        noport();
        winopen("gamma");
	setgamma((float)val);
	}
	else fprintf(stderr,"gamma: gammavalue out of range.\n");
    } else
	printf("%f\n",getgamma());
}
