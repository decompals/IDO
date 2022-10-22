c
c FFT1DU_ex.f
c
c  	This simple example illustrates the use of the FORTRAN
c	interface to the complib FFT routines to calculate a 
c       real-to-complex and complex-to-complex FFT.
c
c	This program reads the number of samples N for the sequence,
c	generates a real and complex sample input arrays, and calculates 
c	the transform.
c
c
c Copyright 1995, Silicon Graphics, Inc.
c ALL RIGHTS RESERVED
c
c UNPUBLISHED -- Rights reserved under the copyright laws of the United
c States.   Use of a copyright notice is precautionary only and does not
c imply publication or disclosure.
c
c U.S. GOVERNMENT RESTRICTED RIGHTS LEGEND:
c Use, duplication or disclosure by the Government is subject to restrictions
c as set forth in FAR 52.227.19(c)(2) or subparagraph (c)(1)(ii) of the Rights
c in Technical Data and Computer Software clause at DFARS 252.227-7013 and/or
c in similar or successor clauses in the FAR, or the DOD or NASA FAR
c Supplement.  Contractor/manufacturer is Silicon Graphics, Inc.,
c 2011 N. Shoreline Blvd. Mountain View, CA 94039-7311.
c
c THE CONTENT OF THIS WORK CONTAINS CONFIDENTIAL AND PROPRIETARY
c INFORMATION OF SILICON GRAPHICS, INC. ANY DUPLICATION, MODIFICATION,
c DISTRIBUTION, OR DISCLOSURE IN ANY FORM, IN WHOLE, OR IN PART, IS STRICTLY
c PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN PERMISSION OF SILICON
c GRAPHICS, INC.
c
c
c 	To build executable:
c    	   % f77 -o FFT1DU_ex FFT1DU_ex.f -lcomplib.sgimath
c
c 	To run:
c    	   % FFT1DU_ex
c
c 	Input:
c    	   4
c
c 	Output:
c    	   Input Real Array of size 4 :
c      	     0.00  1.00  2.00  3.00
c          Input Complex Array of size 8 :
c            0.00  0.00  1.00  0.00  2.00  0.00  3.00  0.00
c   	   FFT of Real Array of size 6 :
c     	     6.00  0.00 -2.00  2.00 -2.00  0.00
c          FFT of Complex Array of size 8
c            6.00  0.00 -2.00  2.00 -2.00  0.00 -2.00 -2.00
c
c
c          % FFT1DU_ex
c
c	Input:
c          5
c
c 	Output:
c          Input Real Array of size 5 :
c            0.00  1.00  2.00  3.00  4.00
c          Input Complex Array of size10 :
c            0.00  0.00  1.00  0.00  2.00  0.00  3.00  0.00  4.00  0.00
c          FFT of Real Array of size 6 :
c            10.00  0.00 -2.50  3.44 -2.50  0.81
c          FFT of Complex Array of size10 :
c            10.00  0.00 -2.50  3.44 -2.50  0.81 -2.50 -0.81 -2.50 -3.44
c 
      PROGRAM FFT1DU_ex
      IMPLICIT NONE
      INTEGER     I, N, SIZE
      PARAMETER  ( SIZE = 8 )
      REAL        FARRAY( 2*((SIZE+2)/2) ), FCOEF( SIZE+15 )
      COMPLEX     CARRAY( SIZE ),           CCOEF( SIZE+15 )
 
c
c     Read the number of samples N.
c
      READ( 5, * ) N
c
c     Generate the real and complex sequences.
c
      DO I = 1, N
         FARRAY( I ) =   REAL( I ) - 1.
	 CARRAY( I ) = ( REAL( I ) - 1., 0.)
      END DO 
 
c
c     Print out input data
c
      WRITE( 6,'( A, I2, A )' ) 
     &       "Input Real Array of size", N, " :"
      WRITE( 6,'( 10F6.2 )' ) ( FARRAY( I ), i = 1, n )
      WRITE( 6,'( A, I2, A )' )
     &       "Input Complex Array of size", 2*N, " :"
      WRITE( 6,'( 10F6.2 )' )
     &       ( REAL( CARRAY( I ) ), IMAG( CARRAY( I ) ),I = 1, N )
   
c
c     Calculate the transforms.
c
      CALL SCFFT1DUI ( N, FCOEF )
      CALL SCFFT1DU  ( -1, N, FARRAY, 1, FCOEF )         
      CALL CFFT1DI   ( N, CCOEF )
      CALL CFFT1D    ( -1, N, CARRAY, 1, CCOEF )

c
c     Print out transforms.
c
      WRITE( 6, '( A, I2, A )' ) 
     &       "FFT of Real Array of size", 2*( ( N+2 )/2 ), " :"
      WRITE( 6, '( 10F6.2 )' ) 
     &       ( FARRAY( I ), I = 1, 2*( ( N+2 )/2 ) )
      WRITE( 6, '( A, I2, A )' )
     &       "FFT of Complex Array of size", 2*N, " :"
      WRITE( 6, '( 10F6.2 )' )
     &       ( REAL( CARRAY( I ) ), IMAG( CARRAY( I ) ), I = 1, N )

      STOP
      END
