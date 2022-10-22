
	program dmxpy
	parameter (n=2000)
	real*8 m(n,n),y(n),x(n)
	do j = 1,n
	  do i = 1,n
	    m(i,j) = i*j*1d-05
	    x(i) = i*j*3d-04
	  enddo	
	enddo

C	call clear_timer(1)
C	call start_timer(1)

	call ji(m,x,y)

C	call stop_timer(1)
C	call print_timer(1,'dmxpyji 2000')

	print *,'y(1)='
	print '(g15.10)', y(1)

C	call clear_timer(2)
C	call start_timer(2)

	call ij(m,x,y)

C	call stop_timer(2)
C	call print_timer(2,'dmxpyij 2000')

	print *,'y(1)='
	print '(g15.10)', y(1)

      stop
      end

	subroutine ij(m,x,y)
	parameter (n=2000)
	real*8 m(n,n),y(n),x(n)
         
        do i = 1, n
          do j = 1, n
            y(i) = y(i) + x(j) * m(i, j)
         enddo
	enddo

	return
	end

	subroutine ji(m,x,y)
	parameter (n=2000)
	real*8 m(n,n),y(n),x(n)
         
        do j = 1, n
          do i = 1, n
            y(i) = y(i) + x(j) * m(i, j)
         enddo
	enddo

	return
	end


