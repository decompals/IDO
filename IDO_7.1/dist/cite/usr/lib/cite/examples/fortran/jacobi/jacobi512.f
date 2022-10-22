      program jacobi512
      parameter (n=512)
      double precision a(n, n), b(n, n)
      real*4 t1, t2(2), t3(2)

C       
C     -- Initializations --
      do i = 1, 512
         do j = 1, 512
            a(j, i) = 12.0
            b(j, i) = 13.0
         enddo
      enddo
      
        
C       -- Jacobi --
      t1 = etime(t2)
      call jacobi(a, b)
      t1 = etime(t3)
      print *, "Time = ", t3(1) - t2(1)
      end
      
      subroutine jacobi(a, b)
      parameter (n=512)
      double precision a(n,n), b(n,n)


      do i = 2, 512 - 1
         do j = 2, 512 - 1
            a(i, j) = 0.25 * (b(i,j) + b(i - 1, j) + b(i + 1, j) + 
     *           b(i, j - 1) + b(i, j + 1))
         enddo
      enddo
      do j = 2, 512 - 1
         do i = 2, 512 - 1
            b(i, j) = a(i, j)
         enddo
      enddo
      end
