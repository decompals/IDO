      subroutine livrm6(b, w)
      parameter(n = 50)                                           
      real*8 b(n,n), w(n)                                          
C      print *,'enter executions'
C      read *,iexec

      iexec = 1000
      it = 0
      ic = 0
      do i = 1,n
         w(i) = 1.0d0
         do j = 1,n
            b(i,j) = i+j
         enddo
      enddo
      it = mclock()
C     
      do iter = 1,iexec
         do i= 2,n
            do k= 1,i-1
               w(i)= w(i) + b(i,k) * w(i-k)
c     ic = ic + 1
c     print *,i,i-k
            enddo
         enddo
      enddo
      it = mclock() - it
      print 10, it/60.0,w(n),ic
 10   format('time for kernel6 = ',f10.4,' seconds',e10.4,i10)
      print 20, (w(i), i = 1,n)
 20   format(5f10.4)
 999  stop
      end
