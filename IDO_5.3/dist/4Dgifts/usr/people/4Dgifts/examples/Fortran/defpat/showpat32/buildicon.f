	subroutine buildicon (icon,iconid)
 
	  integer*2     icon(*)
	  integer       iconid
 
	  integer       genobj
 
	  call defpat(1,32,icon)
	  iconid = genobj()
	  call makeob(iconid)
            call setpat(0)
            call color(5)
 	    call clear
	    call setpat(1)
	    call color(3)
 	    call clear
	  call closeo()
	  return
	end
