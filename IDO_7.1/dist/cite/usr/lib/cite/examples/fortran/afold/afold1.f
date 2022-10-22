	subroutine afold(f1, f2, f3)
	parameter (n=1000 ,n3=1000, n1 = 750, n2 = 100)
      	real*8 f3(1:n),f1(1:n),f2(1:n)
	real t1,t2(2),t3(2)
        do i = 1,n
	    f1(i) = i*1d-04
	    f2(i) = i*3d-03
	enddo
	dt = 1.9d-02
	t1 = etime(t2)
	do j = 1,1000
          do i = 1,n3
            f3(i) = 0.0
            do k = i, n1
              f3(i) = f3(i) + f1(k) * f2(k)
            enddo
          f3(i) = dt * f3(i)
         enddo
	enddo
	t1 = etime(t3)
	print *,"Time = ",t3(1) - t2(1)
        end
