c
c FFTM1D_2D_ex.f
c
c       This simple example illustrates the use of the FORTRAN
c       interface to multiple 1d FFT routines and 2d FFT routines.
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
c       To build executable:
c	   % f77 -o FFTM1D_2D_ex FFTM1D_2D_ex.f -lcomplib.sgimath
c	   % FFTM1D_2D_ex
c	   3
c	   4
c	   Input Array of Size 3x 4 :
c	     0.00  0.01  0.02  0.03
c	     1.00  1.01  1.02  1.03
c	     2.00  2.01  2.02  2.03
c	   2D FFT through multiple 1D FFTS Array of Size 4x 4 :
c	    12.18 -0.06 -0.06 -0.06
c	     0.00  0.06  0.00 -0.06
c	    -6.00  0.00  0.00  0.00
c	     3.46  0.00  0.00  0.00
c	   2D FFTS Array of Size 4x 4 :
c	    12.18 -0.06 -0.06 -0.06
c	     0.00  0.06  0.00 -0.06
c	    -6.00  0.00  0.00  0.00
c	     3.46  0.00  0.00  0.00


      PROGRAM FFTM1D_2D_ex
      IMPLICIT NONE
      INTEGER      I, J, M, N, LD, SIZE
      PARAMETER  ( SIZE = 8 )
      REAL         A1( 2*( (SIZE+2)/2 )*SIZE ),
     1             A2( 2*( (SIZE+2)/2 )*SIZE ),
     2             FCOEF1( SIZE+15 ), FCOEF2( 2*SIZE+30 )
      COMPLEX      CCOEF( SIZE+15 )
 
      READ ( 5, * ) M, N
      LD = 2*( (M+2)/2 )
      DO J = 1, N
         DO I = 1, LD
            A1( I + LD*(J-1) ) = REAL( I-1 ) + 0.01 * REAL( J-1 )
            A2( I + LD*(J-1) ) = REAL( I-1 ) + 0.01 * REAL( J-1 )
         END DO
      END DO 
 
      WRITE( 6, '(A, I2, A, I2, A)' ) 
     &      "Input Array of Size", M, "x", N, " :"
      DO I = 1, M
         WRITE( 6,'( 10F6.2 )' ) ( A1( I + LD*(J-1) ), J = 1, N )
      END DO

      CALL SFFTM1DUI ( M, FCOEF1 )
      CALL CFFTM1DI  ( N, CCOEF  )
      CALL SFFT2DUI  ( M, N, FCOEF2 )
      CALL SCFFTM1DU ( -1, M, N, A1, 1, LD, FCOEF1 )
      CALL CFFTM1D   ( -1, N, LD/2, A1, LD/2, 1, CCOEF )         
      CALL SCFFT2DU  ( -1, M, N, A2, LD, FCOEF2 )         

      WRITE( 6, '( A, A, I2, A, I2, A )' ) 
     1      "2D FFT through multiple 1D FFTS", " Array of Size", 
     2       LD, "x", N, " :"
      DO I = 1, LD
         WRITE( 6,'( 10F6.2 )' ) ( A1( I + LD*(J-1) ), J = 1 ,N )
      END DO
      WRITE( 6,'( A, I2, A, I2, A )' ) "2D FFTS Array of Size",
     &       LD, "x", N, " :"
      DO I = 1, LD
         WRITE( 6, '( 10F6.2 )' ) ( A2( I + LD*(J-1) ), J = 1, N )
      END DO

      STOP
      END
