
        function readicon (icon,ifile)
 
	  integer        ricon
	  integer*2      icon(*)
	  character*(*)  ifile
 
	  integer        i,ICONSZ
	  parameter     (ICONSZ = 64)
 
	  open(unit=5,file=ifile,form='formatted',err=99)
	  read(5,'(i)') (icon(i), i = 1, ICONSZ)
	  goto 199
99	  readicon = -1
  	  write(*,*) 'Error in opening or reading file ',ifile
199       readicon = 0
	  close(unit=5)
	  return
	end
