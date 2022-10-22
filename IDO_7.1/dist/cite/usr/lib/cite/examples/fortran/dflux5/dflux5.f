     	program dflux5  
	implicit double precision (a-h, o-z)
C       FOUR EQUATION MODEL
C       SECOND DIFFERENCES WITH FIXED COEFFICIENT
        common /fil/ radj(194, 34), dis(193, 33), fis0, il, jl
	real*4 t1, t2(2), t3(2)
	
	do j = 1, 33
	  do i = 1, 193
		dis(i, j) = i*1.0 + j*.20
	  enddo
	enddo

	do j = 1, 34
	  do i = 1, 193
		radj(i, j) = j*1.0 + i*.20
	  enddo
	enddo

	fis0 = 5.d0
	jl = 33
	il = 193

c	call clear_timer(1)
c	call start_timer(1)

	do 100 lm = 1, 1000
	call compute()
100	continue

c	call stop_timer(1)
c	call print_timer(1, 'dflux5 1000 iterations')

	print *, 'dis(3,3)='
	print *, dis(3,3)

	stop
	end

	subroutine compute()
	
	implicit double precision (a-h, o-z)
        common /fil/ radj(194, 34), dis(193, 33), fis0, il, jl

	
33      do 34 j = 1, jl
          do 34 i = 2, il
            dis(i, j) = fis0 * (radj(i, j + 1) + radj(i, j))
34      continue


	return
	end
