      subroutine simple ()
      implicit real*8(a-h,o-z)
      common /glob/ b(100, 100)
      real*8 b

      do i = 1, 100, 16
         do j = 1, 100, 16
            b(i,j) = 0
         enddo
      enddo

      return
      end
