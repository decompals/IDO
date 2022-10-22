      
      PROGRAM DGETMAIN

      parameter(n=500)
      parameter(m=500)
      real*8 a(m,n)
      integer ipiv(n)
      integer result

      do j = 1,n
         do i = 1, n
            a(i,j) = min(i,j)
         enddo
      enddo

      call dgetf3(m, n, a, 1, ipiv, result)

      if (result .eq. 0) then 
         write *,"Successfully LU decomposed"
      else if (result .gt. 0) then   
         write *,'The factorization has been completed but the'
         write *, 'factor U is singular and divison by zero'
         write *, 'will occur if it is used to solve a system'
         write *, 'of equations'
c         write *,'is singular and division by zero will occur if it is used'
c         write *,'to solve a system of equations.'
      else
           write *, 'Not factorized'
      endif
      
      end


      SUBROUTINE DGETF3( M, N, A, LDA, IPIV, INFO )
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( N )
      DOUBLE PRECISION   A( LDA, N )
*     ..
*
*  Purpose
*  =======
*
*  DGETF3 computes an LU factorization of a general m-by-n matrix A
*  using partial pivoting with row interchanges.
*
*  The factorization has the form
*     A = P * L * U
*  where P is a permutation matrix, L is lower triangular with unit
*  diagonal elements (lower trapezoidal if m > n), and U is upper
*  triangular (upper trapezoidal if m < n).
*
*  This is the right-looking blocked hand optimized version.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the m by n matrix to be factored.
*          On exit, the factors L and U from the factorization
*          A = P*L*U; the unit diagonal elements of L are not stored.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  IPIV    (output) INTEGER array, dimension (min(M,N))
*          The pivot indices; for 1 <= i <= min(M,N), row i of the
*          matrix was interchanged with row IPIV(i).
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -k, the k-th argument had an illegal value
*          > 0: if INFO = k, U(k,k) is exactly zero. The factorization
*               has been completed, but the factor U is exactly
*               singular, and division by zero will occur if it is used
*               to solve a system of equations.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IMAX, J, K,  NB
      DOUBLE PRECISION   AKK, RAKK
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
      COMMON /BLOCK/ NB
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
*
      DO K=1, MIN(M,N)
*
*     
            AKK = ABS( A(K,K) ) 
            IMAX = K
            DO I = K+1, M
               IF ( ABS(A(I,K)) .GT. AKK ) THEN
                  IMAX = I
                  AKK = ABS(A(I,K))
               ENDIF
            ENDDO
            IPIV(K) = IMAX
*
*           Apply row interchanges from the first column thru the
*           end of the block.
*
            IF ( IMAX .GT. K ) THEN
               DO J = 1, N
                  AKK = A(K,J)
                  A(K,J) = A(IPIV(K),J)
                  A(IPIV(K),J) = AKK
               ENDDO
            END IF
            IF ( ABS( A(K,K) ) .LE. ZERO ) GO TO 30
            RAKK = ONE / A(K,K) 
            DO I = K+1, M
               A(I,K) = A(I,K) * RAKK 
            ENDDO
*     
*           Update the columns within the block.
*
            DO J = K+1, N
               DO I = K+1, M
                  A(I,J) = A(I,J) - A(I,K) * A(K,J) 
               ENDDO
            ENDDO
*
*
      ENDDO
      GO TO 40
*     
   30 CONTINUE
      INFO = J
*
   40 CONTINUE
      RETURN
*
*     End of DGETF3
*
      END
