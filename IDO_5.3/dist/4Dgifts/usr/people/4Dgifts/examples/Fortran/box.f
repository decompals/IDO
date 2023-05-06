c  
c   box.f:
c
c     Rudimentary box drawing example.  program draws a green-edged rectangle
c  using move2i/draw2i.  Press the "Esc"[ape] key to exit.  
c

#include <gl/fgl.h>
#include <gl/fdevice.h>

      integer winid
      integer dev

      call prefpo(100, 800, 100, 600)
      winid = winope('box example', 11)
      call qdevic(ESCKEY)

      call drawbox

   5  continue
      dev = qtest()
      if (dev .ne. 0) then
          dev = qread(val)
      endif
      if (dev .eq. ESCKEY) go to 99
      if (dev .eq. REDRAW) then
           call reshap
           call drawbox
      endif
      go to 5

  99  continue
      call gexit
      stop
      end

      subroutine drawbox
#include <gl/fgl.h>
      call color(BLACK)
      call clear

c  draw box
      call color(GREEN)
      call move2i(200,200)
      call draw2i(200,400)
      call draw2i(600,400)
      call draw2i(600,200)
      call draw2i(200,200)
      return
      end

