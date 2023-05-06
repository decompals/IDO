

	subroutine compute(points,n)

#include "common"


	do i=1,n
	x(i) = points(1,i)
	y(i) = points(2,i)
	enddo

	call minmax(x,n,xmin,xmax)
	call minmax(y,n,ymin,ymax)

	fudge = min(xmax/10.,ymax/10.)

	xmin = xmin - fudge
	xmax = xmax + fudge
        ymin = ymin - fudge
        ymax = ymax + fudge

	xmid = xmax/2.+xmin
	ymid = ymax/2.+ymin

	return
	end

	subroutine minmax(b,n,realmin,realmax)

#include "common"

	real b(LARGE),realmin,realmax

	realmin =  1000000.0
	realmax = -realmin

	do i=1,n
	 realmin=min(b(i),realmin)
	 realmax=max(b(i),realmax)
	enddo
	
	return
	end
