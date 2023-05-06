c
c   shadedpoly.f:
c
c     This program demonstrates a simple implementation of drawing a shaded
c  polygon using pmv/pdr/pclos.  The color ramp that is built runs from 
c  color map indices 32 up to 63.  An extra step is taken to save out the
c  colormap in this range before we overwrite these indices, and then upon
c  exiting, restore this original range of 32 colors.
c     Press the "Esc"[ape] key to exit.
c

#include <gl/fgl.h>
#include <gl/fdevice.h>

      integer i, j, dev, redchanl(32), grechanl(32), bluchanl(32)
      integer*2 val, redval, greval, bluval
 
      call prefsi(500, 500)
      wid = winope('shadedpoly', 10)
      call qdevic(ESCKEY)

c  get the original color values from 32 up to 63 and save them into
c  their three respective arrays
      j = 1
      do i = 32, 63
	 call getmco(i, redval, greval, bluval)
	 redchanl(j) = redval
	 grechanl(j) = greval
	 bluchanl(j) = bluval
	 j = j + 1
      end do

c  now load the magenta color ramp
      j = 0
      do i = 32, 63
	 call mapcol(i, 4*j, 0, 4*j)
	 j = j + 1
      end do
 
c  draw the shaded polygon
      call drawsp
 
c  preform our standard loop of:  1.) get input, 2). process input
 25   continue
      dev = qtest()
      if (dev .ne. 0) then
          dev = qread(val)
      endif
      if (dev .eq. ESCKEY) go to 99 
      if (dev .eq. REDRAW) then
	   call reshap
	   call drawsp
      endif
      go to 25

 99   continue
c  now that we are finished, restore the original color index values
      j = 1
      do i = 32, 63
	 call mapcol(i, redchanl(j), grechanl(j), bluchanl(j))
	 j = j + 1
      end do
      call color(BLACK)
      call clear
      call gexit
      stop
      end



      subroutine drawsp
#include <gl/fgl.h>
      call color(BLACK)
      call clear
      call color(32)
      call pmv(100.0, 100.0, 0.0)
      call color(48)
      call pdr(400.0, 400.0, 0.0)
      call color(63)
      call pdr(400.0, 100.0, 0.0)
      call pclos
      return 
      end
