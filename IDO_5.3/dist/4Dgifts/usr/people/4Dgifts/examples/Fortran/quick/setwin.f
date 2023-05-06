	subroutine setwin(gid,points,n,caxis)

#include "common"

	real aspect,xm,ym,left,right,bottom,top 

	xm = float(XMAXSC)
    	ym = float(YMAXSC)
	aspect = xm/ym

	call compute(points,n)

	left   = xmin
	right  = xmax
	bottom = ymin
	top    = ymax

	call foregr

	call prefpo(50, XMAXSC/2-50,YMAXSC/2-50, YMAXSC-50)
	gid = winope("2D quick graph",14)
	call minsiz(XMAXSC/10, YMAXSC/10)
	call keepas(XMAXSC, YMAXSC)
	call wincon
	call cmode
        call drawmo(normdr)
	call double
        call shadem(flat)
	call zbuffe(.false.)
	call ortho2(left,right,bottom,top)
	call setcol
	call setque
        call gconfi
	call cls

       return
       end

	subroutine setcol

#include "common"
c
c	set any non-standard colors here
c
	call mapcol(1000,120,120,120)

	return
	end
