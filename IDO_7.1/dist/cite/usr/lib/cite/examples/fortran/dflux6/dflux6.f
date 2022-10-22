        program dflux6
	implicit double precision (a-h, o-z)
        common /fil/  fw(193, 33, 4)
        common /flx/ fs(193, 34, 4)


        do n = 1, 4
          do j = 1, 34
            do i = 1, 193
                fs(i, j, n) = i*1.2+n
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

c	call clear_timer(1)
c	call start_timer(1)

	do 100 lm = 1, 200
	call compute()
100	continue

c	call stop_timer(1)
c	call print_timer(1,'dflux6 200 iterations')

	print *,'fw(193,33,4)='
	print *,fw(193,33,4)

	stop
	end

	subroutine compute()
	implicit double precision (a-h, o-z)
        common /fil/  fw(193, 33, 4)
        common /flx/ fs(193, 34, 4)
	

	do 40 n = 1, 4 
          do 40 j = 2, 33 
            do 40 i = 2, 193
              fw(i, j, n) = fw(i, j, n) - fs(i, j, n) + fs(i, j - 1, n)
40      continue


	return

	end
