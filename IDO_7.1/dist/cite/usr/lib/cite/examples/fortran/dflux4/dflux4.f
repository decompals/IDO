        program dflux4
	implicit double precision (a-h, o-z)
C       FOUR EQUATION MODEL
C       BLENDED SECOND AND FOURTH DIFFERENCES
        common /fil/  fw(193, 33, 4)
        common /flx/ fs(193, 34, 4)
        common /add/ dw(194, 34, 4), dis2(193, 33), dis4(193, 33)
	real t1, t2(2), t3(2)
C
	do n = 1, 4
	  do j = 1, 34
	    do i = 1, 193
		fs(i, j, n) = i*1.2+n
		dw(i, j, n) = i*3.0 + j*1.0
	    enddo
	  enddo
	enddo
     
	do n = 1, 4
	  do j = 1, 33
	    do i = 1, 193
		fw(i, j, n) = i*1.2+n
	    enddo
	  enddo
	enddo

	  do j = 1, 33
	    do i = 1, 193
		dis2(i, j) = i*1.2+j
		dis4(i, j) = j*1.2 + i
	    enddo
	  enddo

c	call clear_timer(1)
c	call start_timer(1)


	do 100 lm = 1, 100
	call compute()
100	continue

c	call stop_timer(1)
c	call print_timer(1,'dflux4 100 iterations')

	print *,'fs(1,1,1)='
	print *, fs(1,1,1)

	print *,'dw(1,1,1)='
	print *, dw(1,1,1)

	print *,'fw(1,1,1)='
	print *, fw(1,1,1)

	stop
	end

	subroutine compute()

	implicit double precision (a-h, o-z)
        common /fil/  fw(193, 33, 4)
        common /flx/ fs(193, 34, 4)
        common /add/ dw(194, 34, 4), dis2(193, 33), dis4(193, 33)
        do 60 n = 1, 4
          do 52 i = 2, 193
            fs(i, 1, n) = fs(i, 2, n)
            fs(i, 33 + 1, n) = fs(i, 33, n)
52        continue
          do 54 j = 2, 33
            do 54 i = 2, 193
              dw(i, j, n) = fs(i, j + 1, n) - 2. * fs(i, j, n) + fs(i, j
     * - 1, n)
54        continue
          do 56 i = 2, 193
            dw(i, 1, n) = 0.
56        continue
          do 58 j = 1, 33
            do 58 i = 2, 193
              fs(i, j, n) = dis2(i, j) * fs(i, j, n) - dis4(i, j) * dw(i
     *, j, n)
58        continue
          do 60 j = 2, 33
            do 60 i = 2, 193
              fw(i, j, n) = fw(i, j, n) - fs(i, j, n) + fs(i, j - 1, n)
60      continue


	return	
	end
