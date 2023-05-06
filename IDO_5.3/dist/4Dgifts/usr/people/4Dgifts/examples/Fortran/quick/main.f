	subroutine mainlo(gid,points,n,caxis)

#include "common"
#include <gl/fdevice.h>

 	integer dev
	integer*2 val

 10   	continue

		dev = qtest()
		if (dev.ne.0) dev = qread(val)
		if (dev.eq.ESCKEY) goto 99
		if (dev.eq.REDRAW) then
			call winset(gid)
			call reshap
			call cls
			call drawsc(points,n,caxis) 
		endif 

	call winset(gid)
	call drawsc(points,n,caxis)

	goto 10

 99     return 
	end

