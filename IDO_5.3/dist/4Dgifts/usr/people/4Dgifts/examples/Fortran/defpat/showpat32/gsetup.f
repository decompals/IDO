	function gsetup ()

#        include <fdevice.h>

	  integer      gsetup

	  integer      i,winope,winatt

	  call prefsi(500, 500)
	  gsetup = winope('showpat',7)
	  call wintit('showpattern 32x32',17)
	  call qdevic(ESCKEY)
	  return
	end
