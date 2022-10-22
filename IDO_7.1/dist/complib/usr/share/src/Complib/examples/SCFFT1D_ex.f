c
c SCFFT1D_ex.f
c
c	This simple example illustrates the use of the FORTRAN
c	interface to the complib 1d FFT routines to perform a 
c	circular shift.
c
c       This program reads the number of samples N for the sequence,
c	and the offset. 
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
c	   % f77 -o SCFFT1D_ex SCFFT1D_ex.f -lcomplib.sgimath
c
c	Tu run executable:
c	   % SCFFT1D_ex
c
c       Input:
c	   7
c	   2
c
c       Output:
c	   Input Array of size 7 :
c	     0.00  1.00  2.00  3.00  4.00  5.00  6.00
c	   Filter of size 7 :
c	     0.00  0.00  1.00  0.00  0.00  0.00  0.00
c	   Scaled Inverse FFT of Array.Filter of size 7 :
c	     5.00  6.00  0.00  1.00  2.00  3.00  4.00
c
c
c

      PROGRAM SCFFT1D_ex
      IMPLICIT NONE
      INTEGER      i, N, SIZE, OFFSET
      PARAMETER  ( SIZE = 8 )
      REAL         ARRAY( 2*((SIZE+2)/2) ), FILTER( 2*((SIZE+2)/2) ),
     &             COEF ( SIZE+15 )

 
c
c     Read in number of samples N and OFFSET.
c
      READ( 5,* ) N, OFFSET

c
c     Generate smaples.
c
      DO I = 1, N
        ARRAY ( I ) =  REAL( I ) - 1.
        FILTER( I ) =  0. 
      END DO 

      FILTER( OFFSET+1 ) = 1.0
 
c
c     Print out input samples.
c
      WRITE( 6,'( A, I2, A )' ) "Input Array of size",N," :"
      WRITE( 6,'( 10F6.2 )' )   ( ARRAY( I ), I = 1, N )
      WRITE( 6,'( A,I2,A )' )   "Filter of size", N, " :"
      WRITE( 6,'( 10F6.2 )' )   ( FILTER( I ), I = 1, N )
   
c
c     Calculate circular shift.
c
      CALL SCFFT1DUI ( N, COEF )
      CALL SCFFT1DU  ( -1, N, ARRAY, 1, COEF )         
      CALL SCFFT1DU  ( -1, N, FILTER, 1, COEF )         
      CALL SPROD1DU  ( N, ARRAY, 1, FILTER, 1 )
      CALL CSFFT1DU  ( 1, N, ARRAY, 1, COEF ) 
      CALL SSCAL1D   ( N, 1./REAL(N), ARRAY, 1 )

c
c     Print out results.
c
      WRITE( 6,'( A, I2, A )' ) 
     &      "Scaled Inverse FFT of Array.Filter of size", N, " :"
      WRITE( 6,'( 10F6.2 )' ) ( ARRAY( I ), I = 1, N )

      STOP
      END
