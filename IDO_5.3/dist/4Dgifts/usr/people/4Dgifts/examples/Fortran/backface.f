c
c    backface.f:
c
c      This program illustrates using the GL command backface to draw a
c  cube in 3-space with its backsides properly hidden.  Note the use of
c  push/pop -matrix to draw the cube's six sides using only one "side"
c  over and over again in conjunction with translate and rotate.
c      Moving the mouse will rotate the cube about its own center.
c      Press the "Esc"[ape] key to exit.
c       

#include <gl/fgl.h>
#include <gl/fdevice.h>

      integer cubesz, winid
      integer dev
      real    ymax
      data cubesz/200/
            
      ymax   = ymaxsc*1.0
      qbsize = cubesz*1.0

      winid = winope('backface', 8)
      call double
      call gconfi
      call ortho(-ymax, ymax, -ymax, ymax, -ymax, ymax)
            
      call qdevic(ESCKEY)
      call makeob(1)
      call rectfi(-cubesz, -cubesz, cubesz, cubesz)
      call closeo
            
c  define a cube 
      call makeob(2)

c  front face
      call pushma
      call transl(0.0, 0.0, qbsize)
      call color(RED)
      call callob(1)
      call popmat
              
c  right face
      call pushma
      call transl(qbsize, 0.0, 0.0)
      call rotate(900, 'y')
      call color(GREEN)
      call callob(1)
      call popmat
         
c  back face
      call pushma
      call transl(0.0, 0.0, -qbsize)
      call rotate(1800, 'y')
      call color(BLUE)
      call callob(1)
      call popmat
         
c  left face
      call pushma
      call transl(-qbsize, 0.0, 0.0)
      call rotate(-900, 'y')
      call color(CYAN)
      call callob(1)
      call popmat
         
c  top face
      call pushma
      call transl(0.0, qbsize, 0.0)
      call rotate(-900, 'x')
      call color(WHITE)
      call callob(1)
      call popmat
         
c  bottom face
      call pushma
      call transl(0.0, -qbsize, 0.0)
      call rotate(900, 'x')
      call color(YELLOW)
      call callob(1)
      call popmat
      call closeo
         
c  turn on back facing polygon removal
      call backfa(1)

c  keep looping while, testing for ESCKEY (i.e. ready to exit) or REDRAW
 25   continue
      dev = qtest()
      if (dev .ne. 0) then
         dev = qread(val)
      endif
      if (dev .eq. ESCKEY) go to 99 
      if (dev .eq. REDRAW) then
	 call reshap 
      endif
      call pushma
      call rotate(5*getval(MOUSEX), 'x')
      call rotate(5*getval(MOUSEY), 'y')
      call color(BLACK)
      call clear
      call callob(2)
      call popmat
      call swapbu
      go to 25
 99   continue

      call backfa(0)
      call gexit
      stop
      end
