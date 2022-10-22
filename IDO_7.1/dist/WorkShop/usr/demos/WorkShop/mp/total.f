      program driver 
      implicit none
      integer iold(100,10), inew(100,10),i,j
      double precision  aggregate(100, 10),result
      common /work/ aggregate
      call total(100, 10, iold, inew)
      do 20 j=1,10
      do 10 i=1,100
      result=result+aggregate(i,j)
 10   continue
 20   continue
      write(6,*)' result=',result
      stop
      end


      subroutine total(n, m, iold, inew)
      implicit none
      integer n, m
      integer iold(n,m), inew(n,m)
      double precision  aggregate(100, 10)
      common /work/ aggregate
      integer i, j, num, ii, jj
      double precision tmp
      
C$DOACROSS LOCAL(I,II,J,JJ,NUM)
      do j = 2, m-1
        do i = 2, n-1
      	num = 1
      	if (iold(i,j) .eq. 0) then
      	  inew(i,j) = 1
      	else
      	  num = iold(i-1,j) + iold(i,j-1) + iold(i-1,j-1) +
     &   	iold(i+1,j) + iold(i,j+1) + iold(i+1,j+1)
	      	if (num .ge. 2) then
	        	inew(i,j) = iold(i,j) + 1
	      	else
	      		inew(i,j) = max(iold(i,j)-1, 0)
	      	end if
	end if
      	ii = i/10 + 1
      	jj = j/10 + 1
      	aggregate(ii,jj) = aggregate(ii,jj) + inew(i,j)
        end do
      end do
      end
