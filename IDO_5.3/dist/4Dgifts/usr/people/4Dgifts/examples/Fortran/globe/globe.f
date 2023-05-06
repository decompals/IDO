	program globe

c	  globe: display a globe using a converted earth centered world
c	         coordinate data base.
 	       
c	         grant (slug).

	  logical  gsetup
	  integer  makeglobe
	  character*(10) c

	  if (.not. gsetup()) then
	    goto 99
	  else
	    call showglobe(makeglobe())
	    call gexit
	  endif
99	end
