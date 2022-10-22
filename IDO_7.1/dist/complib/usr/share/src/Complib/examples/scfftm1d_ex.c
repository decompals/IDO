/*
 * scfftm1d_ex.c
 *
 *       This simple example illustrates the use of the FORTRAN
 *       interface to a real-to-complex multiple FFT routines by
 *       calculating a circular shift.
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
 *         % cc -o scfftm1d_ex scfftm1d_ex.c -lcomplib.sgimath -lm
 *
 *      To run executable:
 *	   % scfftm1d_ex 3 4
 *	   Printing " Input Array " of size 3 x 4 :
 *	       0.00    0.01    0.02    0.03
 *	       1.00    1.01    1.02    1.03
 *	       2.00    2.01    2.02    2.03
 *	   Printing " Loop on 1D FFTs - stride 1  " of size 4 x 4 :
 *	       3.00    3.03    3.06    3.09
 *	       0.00    0.00    0.00    0.00
 *	      -1.50   -1.50   -1.50   -1.50
 *	       0.87    0.87    0.87    0.87
 *	   Printing " Multiple1D FFTs - stride 1  " of size 4 x 4 :
 *	       3.00    3.03    3.06    3.09
 *	       0.00    0.00    0.00    0.00
 *	      -1.50   -1.50   -1.50   -1.50
 *	       0.87    0.87    0.87    0.87
 *
 */

#include <stdio.h>
#include <fft.h>

void print_2D_array( int M, int N, char string[], float array[], int ld) {
    int i, j;
    printf( "Printing \" %s \" of size %d x %d :\n", string, M, N);
    for( i = 0 ; i < M ; i++, array++) {
      for( j= 0; j < N ; j++ ) printf(" %7.2f", array[j*ld]);
      printf("\n");
    }
}


main( int argc, char *argv[]) {

#define SIZE 8
    int i, j, M, N, offset;
    float Array1[2*((SIZE+2)/2)*SIZE], Array2[2*((SIZE+2)/2)*SIZE], *coef;
    M = atoi( argv[1]); N = atoi( argv[2]);

    for( j = 0 ; j < N ; j ++ )
      for ( i = 0 ; i < (M+2) ; i++) 
        Array1[i+(M+2)*j] = Array2[i+(M+2)*j] = (float)i + 0.01 * (float)j;
    print_2D_array( M, N, "Input Array", Array1, M+2);

    coef = sfft1dui( M, NULL);
    for( j = 0 ; j < N ; j++)
        scfft1du( -1, M, Array1+j*(M+2), 1, coef);
    scfftm1du( -1, M, N, Array2, 1, (M+2), coef);

    print_2D_array( 2*((M+2)/2),N, "Loop on 1D FFTs - stride 1 ",Array1,M+2);
    print_2D_array( 2*((M+2)/2),N, "Multiple1D FFTs - stride 1 ",Array2,M+2);
}
