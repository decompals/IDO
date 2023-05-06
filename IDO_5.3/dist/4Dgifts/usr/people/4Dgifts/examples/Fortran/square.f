c
c    square.f:
c
c      This program demostrates the drawing of a square grid of points
c  centered in a window where the viewport (or screen space) and the 
c  orthographic projection (or world space) share a one-to-one mapping.
c  This is accomplished by centering each world space whole-numbered
c  value directly in the middle of each screen space pixel.  In a simpler 
c  case, if the viewport was only 1 pixel big, altho it has a size of 1
c  unit, its index value is zero--thus in order to center a world space 
c  location of zero directly in the middle of this pixel, we would have 
c  it start at -0.5 and run to 0.5.
c      Press the "Esc"[ape] key to exit.
c

#include <gl/fgl.h>
#include <gl/fdevice.h>
 
      integer dev
      integer xsize, ysize
      integer*2 val
 
      xsize = 500
      ysize = 500
      call prefsi(xsize, ysize)
      wid = winope('square', 6)
      call qdevic(ESCKEY)

      call viewpo(0,xsize-1,0,ysize-1)
      call ortho2(-0.5, xsize-0.5, -0.5, ysize-0.5)
      call transl(25.0, 25.0, 0.0)

      call drwgrd

 25   continue
      dev = qtest()
      if (dev .ne. 0) then
          dev = qread(val)
      endif
      if (dev .eq. ESCKEY) go to 99 
      if (dev .eq. REDRAW) then
           call reshap 
           call drwgrd
      endif
      go to 25
 99   continue

      call gexit
      stop
      end



      subroutine drwgrd
#include <gl/fgl.h>
      integer i, j
      call color(BLACK)
      call clear
      call color(YELLOW)
      do i = 0, 90
          do j = 0, 90
              call pnti(i*5, j*5,0)
          end do
      end do
      return
      end
