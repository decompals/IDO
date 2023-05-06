      function pickfatbit (object,ixorig,iyorig)

        logical      pickfatbit
	integer      object,ixorig,iyorig

#	include <fgl.h>
#	include <fdevice.h>

	integer      IXMAX,IYMAX
	parameter   (IXMAX = 32,IYMAX = 32)
	real         wx,wy
	integer      sy,sx,fatobj

	common      /OBJECTS/ fatobj

        pickfatbit = .FALSE.
        call mapw2(fatobj,getval(266)-ixorig,getval(267)-iyorig,wx,wy)
  	sx = IXMAX-int(wx)
  	sy = int(wy+0.95)
	if (sx.gt.0 .and. sx.le.IXMAX .and. sy.ge.0 .and. sy.le.IYMAX) then
	   object = 1+(sx-1+(sy-1)*32)
           pickfatbit = .TRUE.
	endif
        return
      end
