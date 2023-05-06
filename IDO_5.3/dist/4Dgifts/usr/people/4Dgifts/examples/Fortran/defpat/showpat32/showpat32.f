	program defpat32

#        include <fdevice.h>
	  integer       TOTCON
	  parameter    (TOTCON = 32*32)
 
	  logical       getfile
	  integer*2     icon(TOTCON),state
          integer       iconid,gsetup,device,qread,readicon
	  character*80  infile

          if (.not. getfile(infile)) goto 99
	  if (gsetup() .eq. -1) goto 99
	  if (readicon(icon,infile) .eq. -1) goto 99
	  call buildicon(icon,iconid)
	  call qenter(528,1)
5	  continue
	    device = qread(state)
	    if (device .eq. REDRAW) then
               call reshap
	       call callob(iconid)
	    elseif (device .eq. ESCKEY) then
	       call gexit
	       goto 99
	    endif
	  goto 5
99	end
