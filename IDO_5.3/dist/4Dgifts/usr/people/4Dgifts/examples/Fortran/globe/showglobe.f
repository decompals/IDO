        subroutine showglobe (globeobj)

	  integer  globeobj

#	  include <fgl.h>
#	  include <fdevice.h>

	  logical   active
          integer*2 val
          integer*4 event
	  integer*4 nobjct,ifac,jfac,kfac
	  integer*4 xorig,yorig,xsize,ysize
	  integer*4 curmx,curmy,prevmx,prevmy
	  real      sfac,tfac,rfac

	  data ifac /0/,jfac /0/, kfac /0/
	  data sfac /1.0/, tfac /0.0/, rfac /0.0/

	  call getori(xorig, yorig)
	  call getsiz(xsize, ysize)
	  curmx = xsize/2
	  curmy = ysize/2

5	  continue
	    call color(BLACK)
	    call clear
	    if (getbut(ONEKEY)) then
	      sfac = (getval(MOUSEX) - xorig)/512.0*6.0
	    elseif (getbut(TWOKEY)) then
C             prevmx = curmx
C             prevmy = curmy
C             curmx = getval(MOUSEX)-xorig
C             curmy = getval(MOUSEY)-yorig
C             rfac = (curmx - prevmx) * 100.0 / xsize 
C             tfac = (curmy - prevmy) * 100.0 / ysize 
              rfac = (getval(MOUSEX) - xorig)/123.0-2.0
              tfac = (getval(MOUSEY) - yorig)/153.0-2.0
	    elseif (getbut(THREEK)) then
	      jfac = 5*(getval(MOUSEY)-384)
	      ifac = 5*(getval(MOUSEX)-512)
	    endif
	    call color(BLUE)
	    call pushma
	    call scale(sfac,sfac,sfac)
	    call transl(rfac,tfac,0.0)
	    call circf(0.0,0.0,1.0)
	    call rotate(jfac, 'x')
	    call rotate(jfac, 'z')
	    call rotate(ifac, 'y')
	    call callob(globeobj)
	    if (qtest() .gt. 0) then
	       event = qread(val)
	       if (event .eq. REDRAW) then
		 call reshap()
		 call frontb(.TRUE.)
		 call color(BLACK)
		 call clear
		 call callob(globeobj)
		 call frontb(.FALSE.)
	         call getori(xorig, yorig)
	       elseif (event .eq. ESCKEY) then
	         goto 99
	       elseif (event .eq. INPTCH) then
		 if (val .gt. 0) then
	           active = .TRUE.
	         else
	           active = .FALSE.
	           call frontb(.TRUE.)
	           call color(BLACK)
	           call clear
                   call callob(globeobj)
	           call frontb(.FALSE.)
	         endif
	       endif
	       if (.not. active) then
20	         if (qtest() .eq. 0) then
	            call swapbu
	            goto 20
	         endif
	       endif
	    endif
            call popmat
	    call swapbu
          goto 5
99	  return
	end
