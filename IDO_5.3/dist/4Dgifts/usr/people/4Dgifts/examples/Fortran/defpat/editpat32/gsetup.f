      function gsetup ()

#	 include <fgl.h>
#	 include <fdevice.h>

	 integer      gsetup

	 integer      IXMAX,IYMAX
	 parameter   (IXMAX = 32,IYMAX = 32)
	 real         delta
	 integer      i,j,iob,fatobj
	 character*20 infile,outfile

	 common    /FILENAMES/ infile,outfile
	 common    /OBJECTS/   fatobj

	 call keepas(1,1)
	 call prefsi(500,500)
	 gsetup = winope('editpat32',9)
	 call wintit('Defpattern Editor (32x32)',25)
	 call gconfi
         call winatt
	 call qdevic(MOUSE1)
	 call qdevic(MOUSE3)
	 fatobj = genobj()
	 call makeob(fatobj)
	   call ortho2(-0.50, 32.50, -0.50, 34.25)
	 call closeo
	 iob = 0
	 delta = 0.10
	 do 15 i = 1, IYMAX
	   do 10 j = IXMAX, 1, -1
	     iob = iob+1
	     call makeob(iob)
	       call rectf(j-1+delta, i-1+delta, j-delta, i-delta)
	     call closeo
10         continue
15       continue
	 return
      end
