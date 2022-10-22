
      program chlsky
c
c     Program to solve the linear system Ax = b,
c         where A is dense symmetric positive definite
c
c     A is constructed randomly, subject to above constraint
c     b is constructed such that x = [1,1,...,1]^T
c     Results are checked by computing |Ax - b| (infinity norm)
c
c     Method used is Cholesky factorization, gaxpy version
c         followed by forward and back substitution
c     

      parameter(NMAX=512)
      double precision a(NMAX,NMAX), aa(NMAX,NMAX), x(NMAX), b(NMAX)
      double precision z(NMAX)
      double precision sumz, s, alpha, t, resid, residn, normx, norma
      double precision dsqrt, dabs, dmax1, eps
      double precision epslon
      real t1, t2(2), t3(2)

c     initialization
      init = 1325
      norma = 0.0d0
      sumz = 0.0d0
      do i = 1, NMAX
         init = mod( 3125*init, 65536 )
         z(i) = (init - 32768.0d0) / 16384.0d0
         sumz = sumz + z(i)*z(i)
      enddo
      init = mod( 3125*init, 65536 )
      s = (init + 1.0d0) / 16384.0
      sumz = dsqrt( sumz )
      alpha = 0.0d0
      do i = 1, NMAX
         z(i) = z(i) / sumz
         alpha = alpha + z(i)*z(i)*s
      enddo
      do i = 1, NMAX
         x(i) = 0.0d0
      enddo
      do j = 1, NMAX
         a(j,j) = s - 4 * z(j) * z(j) * (s-alpha)
         aa(j,j) = a(j,j)
         norma = dmax1( dabs(a(j,j)), norma )
         x(j) = x(j) + a(j,j)
         t = a(j,j)
         do i = j+1, NMAX
            a(i,j) = - 4 * z(i) * z(j) * (s-alpha)
            aa(i,j) = a(i,j)
            t = t + a(i,j)
            x(i) = x(i) + a(i,j)
            norma = dmax1( dabs(a(i,j)), norma )
         enddo
         x(j) = x(j) + t
      enddo

c     cholesky factorization, gaxpy version
      t1 = etime(t2)

      do j = 1, NMAX
         do i = j, NMAX
            do k = 1, j-1
               a(i,j) = a(i,j) - a(i,k)*a(j,k)
            enddo
         enddo
         a(j,j) = dsqrt( a(j,j) )
         t = 1.0d0 / a(j,j)
         do i = j+1, NMAX
            a(i,j) = a(i,j) * t
         enddo
      enddo

      t1 = etime(t3)
      print *, "Time = ", t3(1) - t2(1)

c     solve the system
c     lower triangular solve
      do k = 1, NMAX
         t = x(k)
         do i = k+1, NMAX
            x(i) = x(i) + t * a(i,k)
         enddo
      enddo

c     upper triangular solve
      do kk = 0, NMAX
         k = NMAX - kk
         x(k) = x(k)/a(k,k)
         t = -x(k)
         do i = 1, k-1
            x(i) = x(i) + t * a(k,i)
         enddo
      enddo

c     check results
      do j = 1, NMAX
         t = x(j)
         s = aa(j,j) * t
         do i = j+1, NMAX
            b(i) = b(i) - aa(i,j)*t
            s = s + aa(i,j)*x(i)
         enddo
         b(j) = b(j) - s
      enddo
      resid = 0.0d0
      normx = 0.0d0
      do i = 1, NMAX
         resid = dmax1( resid, dabs(b(i)) )
         normx = dmax1( normx, dabs(x(i)) )
      enddo

c     calculate epsilon
      eps = EPSLON (X)

c      t1 = 4.0d0/3.0d0
c   10 t2 = t1 - 1.0d0
c      t3 = t2 + t2 + t2
c      eps = dabs(t3-1.0d0)
c      if (eps .eq. 0.0d0) go to 10
c      eps = eps*dabs(1.0d0)

      residn = resid / ( NMAX*norma*normx*eps )
      write (6,100) residn, resid, eps, x(1), x(NMAX)
 100  format( ' RESIDN=',e16.8,'    RESID=',e16.8,'    EPS=',e16.8,
     $     '    X(1)=',e16.8,'    X(N)=',e16.8 )


      stop
      end

      DOUBLE PRECISION FUNCTION EPSLON (X)
      DOUBLE PRECISION X
C
C     ESTIMATE UNIT ROUNDOFF IN QUANTITIES OF SIZE X.
C
      DOUBLE PRECISION A,B,C,EPS
      DOUBLE PRECISION DABS
C
C     THIS PROGRAM SHOULD FUNCTION PROPERLY ON ALL SYSTEMS
C     SATISFYING THE FOLLOWING TWO ASSUMPTIONS,
C        1.  THE BASE USED IN REPRESENTING FLOATING POINT
C            NUMBERS IS NOT A POWER OF THREE.
C        2.  THE QUANTITY  A  IN STATEMENT 10 IS REPRESENTED TO 
C            THE ACCURACY USED IN FLOATING POINT VARIABLES
C            THAT ARE STORED IN MEMORY.
C     THE STATEMENT NUMBER 10 AND THE GO TO 10 ARE INTENDED TO
C     FORCE OPTIMIZING COMPILERS TO GENERATE CODE SATISFYING 
C     ASSUMPTION 2.
C     UNDER THESE ASSUMPTIONS, IT SHOULD BE TRUE THAT,
C            A  IS NOT EXACTLY EQUAL TO FOUR-THIRDS,
C            B  HAS A ZERO FOR ITS LAST BIT OR DIGIT,
C            C  IS NOT EXACTLY EQUAL TO ONE,
C            EPS  MEASURES THE SEPARATION OF 1.0 FROM
C                 THE NEXT LARGER FLOATING POINT NUMBER.
C     THE DEVELOPERS OF EISPACK WOULD APPRECIATE BEING INFORMED
C     ABOUT ANY SYSTEMS WHERE THESE ASSUMPTIONS DO NOT HOLD.
C
C     *****************************************************************
C     THIS ROUTINE IS ONE OF THE AUXILIARY ROUTINES USED BY EISPACK III
C     TO AVOID MACHINE DEPENDENCIES.
C     *****************************************************************
C
C     THIS VERSION DATED 4/6/83.
C
      A = 4.0D0/3.0D0
   10 B = A - 1.0D0
      C = B + B + B
      EPS = DABS(C-1.0D0)
      IF (EPS .EQ. 0.0D0) GO TO 10
      EPSLON = EPS*DABS(X)
      RETURN
      END
