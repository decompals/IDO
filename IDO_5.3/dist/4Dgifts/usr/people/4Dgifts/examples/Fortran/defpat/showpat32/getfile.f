      function getfile (file)

	logical      getfile

	integer      i,iargc
	character*80 file

	 i = iargc()
	 if (i .lt. 1) then
	   write(0,*) 'usage: showpat32 infile'
	   getfile = .FALSE.
	 else
	   getfile = .TRUE.
	   call getarg(1,file)
	 endif
  	 return
	end
