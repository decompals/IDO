/*
 * scfft1d_ex.c
 *
 *       This simple example illustrates the use of the FORTRAN
 *       interface to the complib 1d FFT routines to perform a 
 *       circular shift.
 *
 *       This program requires the number of samples N for the 
 *       sequence and the offset as parameters.
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
 *      To build executable:
 *	   % cc -o scfft1d_ex scfft1d_ex.c -lcomplib.sgimath -lm
 *
 *	To run executable:
 *	   % scfft1d_ex 7 2
 *	   Printing " Input Array " of size 7 :
 *	       0.00    1.00    2.00    3.00    4.00    5.00    6.00
 *	   Printing " Filter  " of size 7 :
 *	       0.00    0.00    1.00    0.00    0.00    0.00    0.00
 *	   Printing " Scaled Inverse FFT of Array.Filter " of size 7 :
 *	       5.00    6.00    0.00    1.00    2.00    3.00    4.00
 */

#include <stdio.h>
#include <fft.h>

void print_array( int N, char string[], float array[]) {
    printf( "Printing \" %s \" of size %d :\n", string, N);
    for( ; N > 0 ; N--)
        printf(" %7.2f", *array++);
    printf("\n");
}


main( int argc, char *argv[]) {

#define SIZE 8
    int i, N, offset;
    float Array[2*((SIZE+2)/2)], Filter[2*((SIZE+2)/2)], *coef;

/*
 *  Number of samples N and OFFSET.
 */
    N = atoi( argv[1]);
    offset = atoi( argv[2]);

/*
 *  Generate smaples.
 */
    for ( i = 0 ; i < N ; i++) {
        Array[i]  = (float)i;
        Filter[i] = 0.0;
    }
    Filter[offset] = 1.0;

/*
 *  Print out input samples.
 */
    print_array( N, "Input Array", Array);
    print_array( N, "Filter ", Filter);

/*
 *  Calculate circular shift.
 */
    coef = sfft1dui( N, NULL);
    scfft1du( -1, N, Array, 1, coef);
    scfft1du( -1, N, Filter, 1, coef);
    sprod1du( N, Array, 1, Filter, 1);
    csfft1du(  1, N, Array, 1, coef);
    sscal1d( N, 1./(float)N, Array, 1);

/*
 *  Print out results.
 *
    print_array( N, "Scaled Inverse FFT of Array.Filter", Array);
}
