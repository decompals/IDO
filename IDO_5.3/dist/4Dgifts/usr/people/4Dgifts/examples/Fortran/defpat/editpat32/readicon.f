      function readicon (obcolr)

	 integer      readicon,obcolr(*)

#	 include <fgl.h>
#	 include <fdevice.h>

	 integer      IXMAX,IYMAX
	 parameter   (IXMAX = 32,IYMAX = 32)
	 logical      there
	 integer*2    data2(IXMAX*2)
	 integer      vector(0:31),data4(IXMAX),iob
	 character*20 infile,outfile

	 common /BITVECTOR/ vector
	 common /FILENAMES/ infile,outfile
         equivalence (data4(1),data2(1))

	 readicon = 0
	 inquire(file = infile, exist = there)
	 if (there) then
	   open(11,file = infile,status='old',form='formatted',err=89)
	   read(11,'(i)',err=99) (data2(i), i = 1, IYMAX*2)
	   close(11)
	   iob = 0
	   do 15 i = 1,IYMAX
	     do 10 j = 1,IXMAX
	       iob = iob+1
	       if (iand(data4(i),vector(j-1)) .gt. 0) obcolr(iob) = YELLOW
10	     continue
15         continue
	   goto 199
	 else
	   goto 199
	 endif
89	 write(*,*) 'error opening input file:',infile
99       write(*,*) 'error reading input file:',infile
	 readicon = -1
199      return
      end
