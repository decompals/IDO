      subroutine savefatbit (obcolr,grid,grid2)

	integer*2    grid2(*)
	integer      obcolr(*),grid(*)

#	include <fgl.h>
#	include <fdevice.h>

	integer      IXMAX,IYMAX
	parameter   (IXMAX=32, IYMAX=32)
	integer      i,j,ic,vector(0:31)
	character*20 infile,outfile

	common      /BITVECTOR/ vector
	common      /FILENAMES/ infile,outfile

        ic = 0
        do 15 i = 1,IYMAX
 	  do 10 j = 1,IXMAX
	    ic = ic+1
	    if (obcolr(ic) .gt. BLACK) grid(i) = ior(grid(i),vector(j-1))
10        continue
15      continue
        open(12,file=outfile,status='unknown',form='formatted',err=99)
        write(12,'(i)') (grid2(i), i = 1, IYMAX*2)
	goto 199
99      continue
        write(*,*) 'error opening output file: ',outfile
199	close(12)
	return
      end
