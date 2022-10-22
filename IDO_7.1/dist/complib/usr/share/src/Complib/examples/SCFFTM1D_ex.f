c
c SCFFTM1D_ex.f
c
c       This simple example illustrates the use of the FORTRAN
c       interface to a real-to-complex multiple FFT routines by
c       calculating a circular shift.
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
c       To build executable:
c	   % f77 -o SCFFTM1D_ex SCFFTM1D_ex.f -lcomplib.sgimath
c
c       To run:
c	   % SCFFTM1D_ex
c
c       Input:
c	   3
c	   4
c
c       Output:
c	   Input Array of size 3x 4 :
c	     0.00  0.01  0.02  0.03
c	     1.00  1.01  1.02  1.03
c	     2.00  2.01  2.02  2.03
c	   Loop on 1D FFTs - stride 1 of size 4x 4 :
c	     3.00  3.03  3.06  3.09
c	     0.00  0.00  0.00  0.00
c	    -1.50 -1.50 -1.50 -1.50
c	     0.87  0.87  0.87  0.87
c	   Multiple 1D FFTs - stride 1 of size 4x 4 :
c	     3.00  3.03  3.06  3.09
c	     0.00  0.00  0.00  0.00
c	    -1.50 -1.50 -1.50 -1.50
c	     0.87  0.87  0.87  0.87
c

      PROGRAM SCFFTM1D_ex
      IMPLICIT NONE
      INTEGER      I, J, M, N, OFFSET, SIZE
      PARAMETER  ( SIZE = 8 )
      REAL         ARRAY1( 2*( (SIZE+2)/2 )*SIZE ),
     1             ARRAY2( 2*( (SIZE+2)/2 )*SIZE ),
     2             COEF  ( SIZE+15 )
 
      READ ( 5,* ) M, N

      DO J = 1, N
         DO I = 1, M+2
            ARRAY1( i+( M+2 )*( j-1 ) ) = REAL( I-1 ) + 0.01*REAL( J-1 )
            ARRAY2( i+( M+2 )*( j-1 ) ) = REAL( I-1 ) + 0.01*REAL( J-1 )
         END DO
      END DO 
 
      WRITE( 6,'( A, I2, A, I2, A )' ) 
     &       "Input Array of size", M, "x", N, " :"
      DO I = 1, M
         WRITE( 6,'( 10F6.2 )' ) ( ARRAY1( i+(M+2)*(J-1) ), J = 1, N )
      END DO
   
      CALL SFFT1DUI( M, COEF )
      DO J = 1, N
         CALL SCFFT1DU ( -1, M, ARRAY1( (J-1)*(M+2)+1 ), 1, COEF )
      END DO
      CALL SCFFTM1DU( -1, M, N, ARRAY2, 1, M+2, COEF )         

      WRITE( 6,'( A, I2, A, I2, A)' ) 
     1      "Loop on 1D FFTs - stride 1 of size", 2*((M+2)/2), "x", N, 
     2      " :"
      DO I = 1, 2*( (M+2)/2 )
         WRITE( 6,'(10F6.2)') ( ARRAY1( I+(M+2)*(J-1) ),J = 1, N )
      END DO
      WRITE( 6,'( A, I2, A, I2, A)' ) 
     1      "Multiple 1D FFTs - stride 1 of size", 2*((M+2)/2), "x", N, 
     2      " :"
      DO I = 1, 2*( (M+2)/2 )
         WRITE( 6,'( 10F6.2 )' ) ( ARRAY2( I+(M+2)*(J-1) ), J = 1, N )
      END DO

      STOP
      END

