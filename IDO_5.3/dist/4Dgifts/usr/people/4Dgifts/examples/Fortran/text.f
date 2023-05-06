c
c   text.f:
c
c    This program demonstrates using the GL command "charstr" in concert
c  with cmov2i (when dealing with screen space).  
c    Press the "Esc"[ape] key to exit.
c

#include <gl/fgl.h>
#include <gl/fdevice.h>

      integer dev
      integer*2 val


      call prefsi(500, 400)
      winid = winope('text', 4)
      call qdevic(ESCKEY)
      call drwtxt

 25   continue
      dev = qtest()
      if (dev .ne. 0) then
          dev = qread(val)
      endif
      if (dev .eq. ESCKEY) go to 99 
      if (dev .eq. REDRAW) then
	   call reshap 
	   call drwtxt
      endif
      go to 25
 99   continue

      call gexit
      stop
      end


      subroutine  drwtxt

#include <gl/fgl.h>

      call color(BLACK)
      call clear
      call color(RED)
      call cmov2i(100,230)
      call charst('The first line is drawn ',24)
      call charst('in two parts. ',14)
      call cmov2i(100,214)
      call charst('This line is 16 pixels lower. ',30)
      return
      end
