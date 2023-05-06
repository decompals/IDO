      subroutine drawicon (obcolr)

	 integer   obcolr(*)

# 	 include <fgl.h>
# 	 include <fdevice.h>

	 real       PIXSCL
	 integer    NOBJS,IXMAX,IYMAX
	 parameter (NOBJS = 1024,IXMAX = 32,IYMAX = 32,PIXSCL = 2.0/IYMAX)
	 integer    pixcnt,i,ixtmp,iytmp
	 real       pixout(2,NOBJS)

	 pixcnt = 0
	 do 10 i = 1, NOBJS
	   if (obcolr(i) .gt. BLACK) then
	     ixtmp = mod(i,IXMAX)
	     iytmp = i/IXMAX+1
	     if (ixtmp .eq. 0) then
	       ixtmp = IXMAX
	       iytmp = iytmp-1
	     endif
	     pixcnt = pixcnt+1
	     pixout(1,pixcnt) = 1.0-ixtmp*PIXSCL+1.0
	     pixout(2,pixcnt) = iytmp*PIXSCL+IYMAX
	   endif
10       continue
 	 call color(CYAN)
 	 call rectf(0.0,32.0,2.0,34.0)
	 call color(MAGENT)
	 do 15 i = 1, pixcnt
	   call pnt2(pixout(1,i),pixout(2,i))
15       continue
	 return
      end
