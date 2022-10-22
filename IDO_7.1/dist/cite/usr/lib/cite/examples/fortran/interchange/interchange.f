	program main
	real*4 t1, t2(2), t3(2)
	parameter(n=1000)
	real*8 a(n,n)
	
	m = 100
	t1 = etime(t2) 
	call interchange(a)
	t1 = etime(t3) 
	print *, "Time = ", t3(1) - t2(1)
	stop
	end


C       This subroutine demos the transformation the compiler performs
C       to improve cache performance by interchanging loops.
C       There are two nested loop. The first loop nest is in the wrong
C       order (it strides throuh memory, since fortran arrays are stored
C       in column major order) and has poor cache performance and the second 
C       loop nest is in the correct order and achieves better cache performance.

	subroutine interchange(a)
	parameter(n=1000)
	real a(n,n)


	do i = 1, 1000
	   do j = 1, 1000
	      a(i,j) = 3.1415926
	   end do
	end do
C      Trick the optimizer
       read *, k
       a(k,k) = 0.0

	do j = 1, 1000
	   do i = 1, 1000
	      a(i,j) = 2.718281828
	   end do
	end do

C      Trick the optimizer
       read *, k
       a(k,k) = 0.0
	end
