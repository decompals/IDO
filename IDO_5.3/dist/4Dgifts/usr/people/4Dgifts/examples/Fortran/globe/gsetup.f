
	function gsetup ()

	   logical gsetup

#	   include <fgl.h>
#	   include <fdevice.h>

	   if (winope('globe', 5) .eq. -1) then
	     gsetup = .FALSE.
	   else
	     gsetup = .TRUE.
	     call keepas(1,1)
	     call wincon
	     call wintit('Globe',5)
	     call double
	     call shadem(0)
	     call qdevic(ESCKEY)
	     call gconfi
	     call ortho(-50.0,50.0,-50.0,50.0,-500.0,50.0)
	     call reshap
	     call lookat(0.0,0.0,50.0,0.0,0.0,0.0,0)
	     call scale(15.0,15.0,15.0)
	   endif
	   return
	end
