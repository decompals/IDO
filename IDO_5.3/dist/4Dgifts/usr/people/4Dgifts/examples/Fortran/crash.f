c
c    crash.f:
c
c      This program uses the entire screen to A). draw a blue planet, and
c  B). draw a yellow "space ship" whenever and wherever the user presses the
c  LEFTMOUSE.  Using gselect to test for when the lower left corner of the
c  space ship is entirely over the planet, the program will exit printing 
c  out " CRASH!!!" on the wsh from which the program was invoked.
c

#include <gl/fgl.h>
#include <gl/fdevice.h>

      integer   dev, count, wid, xorig, yorig
      integer*2 val
      integer*2 buffer(50)
      real      shipx, shipy, shipz

      data iplanet/1/

C  Intialize buffer.
      do i = 1, 50
          buffer(i) = 0
      end do

C  The following 3 statements essentially replace the single statement ginit
      call foregr
      call prefpo(150, 700, 100, 600)
      wid = winope('crash', 5)
      call winset(wid)
      call wincon

      call getori(xorig, yorig)

      call qdevic(LEFTMO)
      call qdevic(RIGHTM)
      call qdevic(ESCKEY)

      call color(BLACK)
      call clear
      call color(BLUE)
      call create(iplanet)
      call callob(iplanet)

C  Loop until LEFTMO button is pressed
  15  continue
      dev = qread(val)
      if (val .eq. 0) go to 15

      if (dev .eq. REDRAW) then
	  call reshap
          call getori(xorig, yorig)
          call color(BLACK)
          call clear
          call color(BLUE)
          call callob(iplanet)
          go to 15

      else if (dev .eq. LEFTMO) then
          shipz = 0
          shipx = getval(MOUSEX) - xorig
          shipy = getval(MOUSEY) - yorig

          call color(YELLOW)
          call rect(shipx, shipy, shipx+20, shipy+10)

          call pushma
          call ortho(shipx, shipx+.05, shipy, shipy+.05, shipz-.05, 
     %                    shipz+.05)
          call initna

          call gselec(buffer,50)

          call loadna(1)
          call pushna(2)
          call callob(iplanet)

          count = endsel(buffer)
          call popmat
 
          if (buffer(2) .eq. 1) then
              type *,'CRASH!!!'
              go to 999
          else
              go to 15
          end if

      else if (dev .eq. RIGHTM) then
          go to 999
      else if (dev .eq. ESCKEY) then
          go to 999
      else
         go to 15
      end if                

 999  continue
      call gexit
      stop
      end

      subroutine create(x)
      integer x
      call makeob(x)
      call circfi(400,300,50)
      call closeo
      return
      end
