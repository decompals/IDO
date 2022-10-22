/*
 *
 * fftm1d_2d_ex.c
 *
 *       This simple example illustrates the use of the FORTRAN
 *       interface to multiple 1d FFT routines and 2d FFT routines.
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
 *      To Build executable:
 *        % cc -o fftm1d_2d_ex fftm1d_2d_ex.c -lcomplib.sgimath -lm
 *
 *      To run executable:
 *	   % fftm1d_2d_ex 3 4
 *	   Printing " Input A " array of size 3 x 4 :
 *	       0.00    0.01    0.02    0.03
 *	       1.00    1.01    1.02    1.03
 *	       2.00    2.01    2.02    2.03
 *	   Printing " 2D FFTs through Multiple1D FFTs  " array of size 4 x 4 :
 *	      12.18   -0.06   -0.06   -0.06
 *	       0.00    0.06    0.00   -0.06
 *	      -6.00    0.00    0.00    0.00
 *	       3.46    0.00    0.00    0.00
 *	   Printing " 2D FFTs  " array of size 4 x 4 :
 *	      12.18   -0.06   -0.06   -0.06
 *	       0.00    0.06    0.00   -0.06
 *	      -6.00    0.00    0.00    0.00
 *	       3.46    0.00    0.00    0.00
 *
 */

#include <stdio.h>
#include <fft.h>

void print_2D_array( int M, int N, char string[], float array[], int ld) {
    int i, j;
    printf( "Printing \" %s \" array of size %d x %d :\n", string, M, N);
    for( i = 0 ; i < M ; i++, array++) {
      for( j= 0; j < N ; j++ ) printf(" %7.2f", array[j*ld]);
      printf("\n");
    }
}


main( int argc, char *argv[]) {

#define SIZE 8
    int i, j, M, N, ld;
    float A1[2*((SIZE+2)/2)*SIZE], A2[2*((SIZE+2)/2)*SIZE], *fCoef1, *fCoef2;
    complex *cCoef;

    M = atoi( argv[1]); N = atoi( argv[2]);

    ld = 2*((M+2)/2);
    for( j = 0 ; j < N ; j ++ )
      for ( i = 0 ; i < ld ; i++) 
        A1[i+ld*j] = A2[i+ld*j] = (float)i + 0.01 * (float)j;

    print_2D_array( M, N, "Input A", A1, ld);

    fCoef1 = sfftm1dui( M, NULL);
    cCoef  = cfftm1di ( N, NULL);
    fCoef2 = sfft2dui( M, N, NULL);
    scfftm1du( -1, M, N, A1, 1, ld, fCoef1);
    cfftm1d( -1, N, ld/2, (complex *)A1, ld/2, 1, cCoef);
    scfft2du( -1, M, N, A2, ld, fCoef2);

    print_2D_array( ld, N, "2D FFTs through Multiple1D FFTs ", A1, ld);
    print_2D_array( ld, N, "2D FFTs ", A2, ld);
}
