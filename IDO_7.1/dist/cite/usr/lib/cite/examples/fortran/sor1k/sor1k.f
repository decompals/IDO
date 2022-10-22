      program sor
	parameter (n=1000)
        real*4 a(n, n)
        
C       -- Initializations --
        do i = 1, n
          do j = 1, n
            a(j, i) = 0.12*i-0.1*j
          enddo
        enddo
C       -- SOR --

c	call clear_timer(1)
c	call start_timer(1)

	call compute(a)

c	call stop_timer(1)
c	call print_timer(1,'sor 1000x1000')

	print *,'a(3,3)='
	print *,a(3,3)

	stop
	end

	subroutine compute(a)
	parameter (n=1000)
        real*4 a(n, n)

         do lm = 1, 10
          do j = 2, n - 1
            do i = 2, n - 1
              a(i, j) = 0.175 * (a(i - 1, j) + a(i + 1, j) + a(i, j - 1)
     * + a(i, j + 1)) + 0.3 * a(i, j)
            enddo
          enddo
         enddo
	return
        end
