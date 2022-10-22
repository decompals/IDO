c
c PSLDLT_ex.f
c
c     This simple example illustrates the use of the FORTRAN
c     interface to PSLDLT (Parallel Sparse LDLT).
c
c     This program reads a sparse matrix from a Harwell-Boeing
c     format input file, performs reordering and symbolic
c     factorization, and then factors the matrix.
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
      program PSLDLT_ex

      integer nn, nnz
      parameter ( nn = 100000 )
      parameter ( nnz = 1000000 )
c
c
c     These three arrays hold the sparse input matrix A.
c     Note that A is a symmetric, stored by columns with only the
c     lower triangle present.
c
c
      integer col(nn+1)
      integer row(nnz)
      real*8 a(nnz)

      real*8 x(nn), b(nn)

      character*80 fname, title, cTmp, format1, format2
      real*8  tol, initNorm, finalNorm, ratio, xEigen
      integer i, n, nz, nts, ns, nIters

      integer factor_nz
      real*8 factor_ops
c
      real*8  dot1, dot2
c
c.... read the data from the input file ( NOT included in example )
c
      write( *, '("Input file name: ",$)' )
      read( *,'(a80)' ) fname
      open( unit = 10, file = fname, status = 'old', err = 998 )
c
      read( 10, '(a80)',             err=999, end=997 ) title
      read( 10, '(a80)',             err=999, end=997 )
      read( 10, '(a28,i14,i14)',     err=999, end=997 ) cTmp, n, nz
      if ( n .gt. nn .or. nz .gt. nnz ) then
         write( *,* ) " ***Error in dimensions: ", n, nn, nz, nnz
         stop
      endif
      read( 10, '(a16,a16)',  err=999, end=997) format1, format2
c
      read( 10, format1, err=999, end=997 ) ( col(i), i = 1, n+1)
      read( 10, format2, err=999, end=997 ) ( row(i), i = 1, nz)


c
c
c     Perform pre-processing (ordering and symbolic factorization)
c     on the input matrix.
c
      call PSLDLT_Preprocess(0, n, col, row, factor_nz, factor_ops)

      print *, 'factor nz = ', factor_nz, ' factor_ops = ', factor_ops

c
c     fill a() and b() with arbitrary values
c

      do i=1,nz
         a(i) = i
      enddo

      do i=1,n
         b(i) = i/n
      enddo


c
c     Factor the input matrix
c
      call PSLDLT_Factor(0, n, col, row, a)


c
c
c     Solve Ax=b for x by performing forward and backward solves, using
c     the factor computed by the earlier PSLDLT_Factor() call.
c
      call PSLDLT_Solve(0, x, b)


      stop

 997  write( *,* ) " *** Error eof ", fname
      stop
c
 998  write( *,* ) " *** Error opening ", fname
      stop
c     
 999  write( *,* ) " *** Error reading ", fname
      stop

      end

