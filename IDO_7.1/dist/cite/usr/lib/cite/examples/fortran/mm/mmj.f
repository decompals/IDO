
	program mmj
	parameter (n=200,n1=1000)
	real*8 c(n,n),a(n,n),b(n,n)
	real*8 c1(n1,n1),a1(n1,n1),b1(n1,n1)
	common a,b,c,a1,b1,c1

	do j = 1,n
	  do i = 1,n
	    a(i,j) = i*j*1d-04
	    b(i,j) = a(i,j)
	  enddo	
	enddo

C	call clear_timer(1)
C	call start_timer(1)

        call mmjik200(a,b,c)

C	call stop_timer(1)
C	call print_timer(1,'mmjik 200')

        print *, 'c(100,100)='
        print *, c(100,100)

	do j = 1,n
	  do i = 1,n
	    a(i,j) = i*j*1d-04
	    b(i,j) = a(i,j)
	  enddo	
	enddo

C	call clear_timer(2)
C	call start_timer(2)

        call mmjki200(a,b,c)

C	call stop_timer(2)
C	call print_timer(2,'mmjki 200')

        print *, 'c(100,100)='
        print *, c(100,100)

	do j = 1,n1
	  do i = 1,n1
	    a1(i,j) = i*j*1d-04
	    b1(i,j) = a1(i,j)
	  enddo	
	enddo

C	call clear_timer(3)
C	call start_timer(3)

        call mmjik1000(a1,b1,c1)

C	call stop_timer(3)
C	call print_timer(3,'mmjik 1000')

        print *, 'c1(1000,1000)='
        print *, c1(1000,1000)

      stop
      end

      subroutine mmjik200(a,b,c)
      parameter (n=200)
      real*8 c(n,n),a(n,n),b(n,n)

      do j = 1, n
           do i = 1,n
            c(i, j) = 0.0d0
            do k = 1,n
               c(i, j) = c(i, j) + a(i, k) * b(k, j)
            enddo
         enddo
      enddo

      return
      end

      subroutine mmjki200(a,b,c)
      parameter (n=200)
      real*8 c(n,n),a(n,n),b(n,n)

        do j = 1, n
           do i = 1, n
              c(i, j) = 0.0d0
           enddo
           do k = 1, n
              do i = 1, n
                 c(i, j) = c(i, j) + a(i, k) * b(k, j)
              enddo
           enddo
        enddo

        return
        end

      subroutine mmjik1000(a,b,c)
      parameter (n=1000)
      real*8 c(n,n),a(n,n),b(n,n)

      do j = 1, n
           do i = 1,n
            c(i, j) = 0.0d0
            do k = 1,n
               c(i, j) = c(i, j) + a(i, k) * b(k, j)
            enddo
         enddo
      enddo

      return
      end

