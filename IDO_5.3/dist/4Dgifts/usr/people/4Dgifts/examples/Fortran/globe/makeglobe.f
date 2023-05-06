	function makeglobe ()

	  integer    makeglobe

#	  include <fgl.h>

	  real       rmap(3,5201)
	  integer    mapobj(100),objnum,i,numpoints
	
	  common    /map/ rmap,numpoints

	  i = 0
	  objnum = 1
5	  if (i .lt. numpoints-1) then
	    mapobj(objnum) = genobj()
	    call makeob(mapobj(objnum))
	      i = i+1
	      call move(rmap(1,i),rmap(2,i),rmap(3,i))
7	      if (rmap(1,i) .lt. 999.0 .and. i .lt. numpoints) then
	        call draw(rmap(1,i),rmap(2,i),rmap(3,i))
	        i = i+1
	        goto 7
	      endif
	      call color(WHITE)
	    call closeo
	    objnum = objnum+1
	    goto 5
	  endif
	  makeglobe = genobj()
	  call makeob(makeglobe)
            do 15 i = 1, objnum
               call callob(mapobj(i))
15           continue
	  call closeo
	  return
	end
