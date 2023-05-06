      subroutine drawfatbit (obcolr,ixorig,iyorig)

#	include <fgl.h>
#	include <fdevice.h>

	integer      obcolr(*),ixorig,iyorig

	integer      NOBJS
	parameter   (NOBJS = 1024)
	integer      i,j
	integer      fatobj

	common      /OBJECTS/ fatobj

	call reshap
        call getori(ixorig,iyorig)
	call color(CYAN)
	call clear
	call rectf(0,0,ixsiz,iysiz)
        call callob(fatobj)
	do 10 i = 1, NOBJS
          call color(obcolr(i))
          call callob(i)
10      continue
	return
      end
