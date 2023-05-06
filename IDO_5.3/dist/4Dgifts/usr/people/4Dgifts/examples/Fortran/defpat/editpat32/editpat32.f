      program editpat32

#	 include <fgl.h>
#	 include <fdevice.h>

	 integer      NOBJS,IXMAX,IYMAX,SAVE,QUIT
	 parameter   (NOBJS = 1024,IXMAX = 32,IYMAX = 32,SAVE = 1,QUIT = 2)
	 logical      validcmd,pickfatbit
	 integer*2    val,grid2(IYMAX*2)
	 integer      menu,fatobj,mkmenu,readicon,ixorig,iyorig,gsetup
	 integer      object,obcolr(NOBJS),dev,grid(IYMAX),vector(0:31)
	 character*20 infile,outfile

	 common /OBJECT/    fatobj
	 common /BITVECTOR/ vector
	 common /FILENAMES/ infile,outfile

	 equivalence (grid(1), grid2(1))

	 data obcolr /NOBJS*0/
         data vector /1,2,4,8,16,32,64,128,256,512,1024,2048,4096,
     #                8192,16384,32768,65536,131072,262144,524288,
     #                1048576,2097152,4194304,8388608,16777216,
     #                33554432,67108864,134217728,268435456,
     #                536870912,1073741824,2147483648/

	 if (.not. validcmd()) goto 99
	 if (gsetup() .eq. -1) goto 99
	 if (readicon(obcolr) .eq. -1) goto 99
	 menu = mkmenu()
	 call drawfatbit(obcolr,ixorig,iyorig)
	 call drawicon(obcolr)
5        continue
	     dev = qread(val)
	     if (dev .eq. REDRAW) then
	         call drawfatbit(obcolr,ixorig,iyorig)
	         call drawicon(obcolr)
	     elseif (dev .eq. MOUSE1) then
	         option = dopup(menu)
	         if (option .eq. SAVE) then
	           call savefatbit(obcolr,grid,grid2)
	         elseif (option .eq. QUIT) then
	           call gexit
	           goto 99
	         endif
	     elseif (dev .eq. MOUSE3) then
	       dev = qread(val)
	       if (pickfatbit(object,ixorig,iyorig)) then
     	         obcolr(object) = ieor(obcolr(object),YELLOW)
	         call color(obcolr(object))
	         call callob(object)
	         call drawicon(obcolr)
	       endif
	     endif
	 goto 5
99    end
