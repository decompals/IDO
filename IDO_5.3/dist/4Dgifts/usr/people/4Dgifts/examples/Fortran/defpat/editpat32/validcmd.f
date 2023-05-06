      function validcmd ()

	logical      validcmd

	integer      i,iargc
	character*20 infile,outfile

        common    /FILENAMES/ infile,outfile

	 i = iargc()
	 if (i .lt. 1) then
	   write(0,*) 'editpat32: infile <outfile>'
	   validcmd = .FALSE.
	 else
	   validcmd = .TRUE.
	   call getarg(1,infile)
	   if (i .eq. 2) then
	     call getarg(2,outfile)
	   else
	     outfile = infile
	   endif
	 endif
  	 return
	end
