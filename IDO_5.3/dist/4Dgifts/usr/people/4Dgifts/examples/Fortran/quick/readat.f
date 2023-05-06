
	subroutine readat(points,n,caxis)

#include "common"

	read (5,*)(caxis(i),i=1,3)
	read (5,9)n
	read (5,*)(points(1,i),points(2,i),i=1,n)
c
c	a quick and incomplete journal file
c	you must put " around any titles or axis
c	prior to using it as an input with (5,*)
c	as an input such as 2d < test.data
c	then after edit 2d < fort.10
c
	write(10,*)(caxis(i),i=1,2)
	write(10,9)n
	write(10,10)(points(1,i),points(2,i),i=1,n)

  9	format(i4)
 10	format(2(f20.9))

c	return
	end
