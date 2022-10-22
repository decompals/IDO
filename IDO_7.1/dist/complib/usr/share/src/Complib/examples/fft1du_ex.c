/*
 * fft1du_ex.c
 *
 * 	This simple example illustrates the use of the C
 *      interface to the complib FFT routines to calculate
 *      a real-to-complex and complex-to-complex FFT.
 *
 *      This program reads the number of samples N for the sequence,
 *      generates a real and complex sample input arrays, and calculates
 *      the transform.
 *
 *
 * Copyright 1995, Silicon Graphics, Inc.
 * ALL RIGHTS RESERVED
 *
 * UNPUBLISHED -- Rights reserved under the copyright laws of the United
 * States.   Use of a copyright notice is precautionary only and does not
 * imply publication or disclosure.
 *
 * U.S. GOVERNMENT RESTRICTED RIGHTS LEGEND:
 * Use, duplication or disclosure by the Government is subject to restrictions
 * as set forth in FAR 52.227.19(c)(2) or subparagraph (c)(1)(ii) of the Rights
 * in Technical Data and Computer Software clause at DFARS 252.227-7013 and/or
 * in similar or successor clauses in the FAR, or the DOD or NASA FAR
 * Supplement.  Contractor/manufacturer is Silicon Graphics, Inc.,
 * 2011 N. Shoreline Blvd. Mountain View, CA 94039-7311.
 *
 * THE CONTENT OF THIS WORK CONTAINS CONFIDENTIAL AND PROPRIETARY
 * INFORMATION OF SILICON GRAPHICS, INC. ANY DUPLICATION, MODIFICATION,
 * DISTRIBUTION, OR DISCLOSURE IN ANY FORM, IN WHOLE, OR IN PART, IS STRICTLY
 * PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN PERMISSION OF SILICON
 * GRAPHICS, INC.
 *
 *	To build executable:
 *	  % cc -o fft1du_ex fft1du_ex.c -lcomplib.sgimath -lm
 *
 *	To run executable:
 *	  % fft1du_ex 4
 *	  Printing " Input Real Array " of size 4 :
 *	     0.00   1.00   2.00   3.00
 *	  Printing " Input Complex Array " of size 8 :
 *	     0.00   0.00   1.00   0.00   2.00   0.00   3.00   0.00
 *	  Printing " FFT of Real Array " of size 6 :
 *	     6.00   0.00  -2.00   2.00  -2.00   0.00
 *	  Printing " FFT of Complex Array " of size 8 :
 *	     6.00   0.00  -2.00   2.00  -2.00   0.00  -2.00  -2.00
 *
 *	  % fft1du_ex 5
 *	  Printing " Input Real Array " of size 5 :
 *	     0.00   1.00   2.00   3.00   4.00
 *	  Printing " Input Complex Array " of size 10 :
 *	     0.00   0.00   1.00   0.00   2.00   0.00   3.00   0.00   4.00   0.00
 *	  Printing " FFT of Real Array " of size 6 :
 *	    10.00   0.00  -2.50   3.44  -2.50   0.81
 *	  Printing " FFT of Complex Array " of size 10 :
 *	    10.00   0.00  -2.50   3.44  -2.50   0.81  -2.50  -0.81  -2.50  -3.44
 *	   
 */

#include <stdio.h>
#include <fft.h>

void print_array( int N, char string[], float array[]) {
    printf( "Printing \" %s \" of size %d :\n", string, N);
    for( ; N > 0 ; N--)
        printf(" %6.2f", *array++);
    printf("\n");
}


main( int argc, char *argv[]) {

#define SIZE 8
    int i, N;
    float       fArray[2*((SIZE+2)/2)], *fCoef;
    complex     cArray[SIZE], *cCoef;

/*
 *  The number of samples N.
 */
    N = atoi( argv[1]);

/*
 *  Generate the real and complex sequences.
 */
    for ( i = 0 ; i < N ; i++) {
        cArray[i].re = fArray[i]  = (float)i;
        cArray[i].im = 0.0;
    }

/*
 *  Print out input data.
 */
    print_array( N,   "Input Real Array",             fArray);
    print_array( 2*N, "Input Complex Array", (float *)cArray);

/*
 *  Calculate the transforms.
 */
    fCoef = sfft1dui( N, NULL);
    cCoef = cfft1di ( N, NULL);
    scfft1du( -1, N, fArray, 1, fCoef);
    cfft1d  ( -1, N, cArray, 1, cCoef);

/*
 *  Print out transforms.
 */
    print_array( 2*((N+2)/2), "FFT of Real Array",             fArray);
    print_array( 2*N,         "FFT of Complex Array", (float *)cArray);

}
