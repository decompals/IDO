	subroutine drawsc(points,n,caxis)

#include "common"

	call cls
	call grid(caxis)
	call color(BLUE)
	call linewi(2)
c
c note postion of advancing subscript 
c note limitation of i < = 256 for v commands
c
	i = 0
	call bgnlin
	do j=1,n
		i=i+1
		if(i.ge.255)then
		call endlin
		i=0
		call bgnlin
		endif
	call v2f(points(1,j))
	enddo
	call endlin

	call swapbu

	return
	end
