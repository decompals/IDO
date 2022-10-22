
      program lup
      parameter (n=300)
      parameter (m=300)
      real*8 a(m,n)
      real*4 t1, t2(2), t3(2)
      do j = 1,n
         do i = 1,m
            a(i,j) = min(i,j)
         enddo
      enddo
c
      t1 = etime(t2)
      call lu(a)
      t1 = etime(t3)
      print *,"Time = ", t3(1) - t2(1)
      stop
      end

      subroutine lu(a)
      parameter (n=300)
      parameter (m=300)
      real*8 a(m,n)
      do k = 1,m-1
         tau = abs(a(k,k)) 
         imax = k
c     
         do i = k+1,m
            if (abs(a(i,k)) .gt. tau) then
               imax = i
               tau = abs(a(i,k))
            endif
         enddo
         do j = k,n
            tau = a(k,j)
            a(k,j) = a(imax,j)
            a(imax,j) = tau
         enddo
c     
c        a0 = a(k,k)
         do i = k+1,m
            a(i,k) = a(i,k)/a(k,k)
         enddo
c
         do j = k+1,n
c           a0 = a(k,j)
            do i = k+1,m
               a(i,j) = a(i,j) - a(i,k) * a(k,j)
            enddo
         enddo
      enddo
c
c     do i = 1,m
c        print 500,(a(i,j), j = 1,n)
c     enddo
c500  format(5f10.4)
      end
