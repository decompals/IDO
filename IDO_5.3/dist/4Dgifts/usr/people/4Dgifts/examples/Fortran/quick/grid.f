
	subroutine grid(caxis)

#include "common"

	real vertx(4,numgd),verty(4,numgd)
c
c add grid lines - first spacing ten in each direction c
c

	xmaxt   = xmax+fudge
	xmint   = xmin-fudge
	ymaxt   = ymax+fudge
	ymint   = ymin-fudge
	xdelta  = (xmax-xmin)/numgd
	ydelta  = (ymax-ymin)/numgd

	call color(BLACK)
	call linewi(1)

	do i=1,numgd
	 xnext = xmin+i*xdelta
	 ynext = ymin+i*ydelta

 	 vertx(1,i) = xmint
	 vertx(2,i) = ynext
 	 vertx(3,i) = xmaxt
	 vertx(4,i) = ynext

 	 verty(1,i) = xnext
	 verty(2,i) = ymint
 	 verty(3,i) = xnext
	 verty(4,i) = ymaxt

	 call bgnlin
	 call v2f(vertx(1,i))
	 call v2f(vertx(3,i))
	 call endlin

	 call bgnlin
	 call v2f(verty(1,i))
	 call v2f(verty(3,i))
	 call endlin

	enddo

	call color(GREEN)
	call cmov2(xmin+fudge,ymin+fudge)
	call charst(caxis(2),40)
	call color(CYAN)
	call cmov2(xmin+fudge,ymax-fudge)
	call charst(caxis(3),40)
	call color(RED)
	call cmov2(xmid-fudge,ymax-fudge)
	call charst(caxis(1),40)

	return
	end

