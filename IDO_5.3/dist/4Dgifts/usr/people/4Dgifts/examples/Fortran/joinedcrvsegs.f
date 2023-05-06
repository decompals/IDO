c 
c    joinedcrvsegs:
c
c      This program draws a series of joined curve segments using all three
c    curve functions:  bezier (drawn in red), cardinal (drawn in green), and
c    b-spline (drawn in blue).  Their control points are drawn in yellow.
c      Press the "Esc"[ape] key to exit.
c

#include <gl/fgl.h>
#include <gl/fdevice.h>

      integer winid, dev
      real bezmat(4,4),carmat(4,4),bspmat(4,4),geom2(3,6)
      integer bezier, cardin, bsplin
      real asizth,msixth,tthrds
      parameter(bezier =1)
      parameter(cardin =2)
      parameter(bsplin =3)
      parameter(asixth =1.0/6.0)
      parameter(msixth =-1.0/6.0)
      parameter(tthrds =2.0/3.0)

      data bezmat /-1.0,3.0,-3.0,1.0,
     +              3.0,-6.0,3.0,0.0,
     +             -3.0,3.0,0.0,0.0,
     +              1.0,0.0,0.0,0.0/

      data carmat/-0.5,1.5,-1.5,0.5,
     +             1.0,-2.5,2.0,-0.5,
     +            -0.5,0.0,0.5,0.0,
     +             0.0,1.0,0.0,0.0/

      data bspmat /msixth, 0.5, -0.5, asixth,
     +             0.5   ,-1.0,  0.5, 0.0,
     +            -0.5,   0.0,   0.5, 0.0,
     +             asixth, tthrds, asixth, 0.0/

      data geom2 /150.0,400.0,0.0,
     +            350.0,100.0,0.0,
     +            200.0,350.0,0.0,
     +            50.0,0.0,0.0,
     +            0.0,200.0,0.0,
     +            100.0,300.0,0.0/

      call keepas(1,1)
      winid = winope('joined curve segments', 21)
      call qdevic(ESCKEY) 

      call ortho(-50.0, 400.0, -50.0, 450.0, -1.0, 1.0)

      call makeob(1)
      call color(BLACK)
      call clear
      call defbas(bezier, bezmat)
      call defbas(cardin, carmat)
      call defbas(bsplin, bspmat)
      call curveb(bezier)
      call curvep(20)
      call color(RED)
      call crvn(6,geom2)

      call curveb(cardin)
      call color(GREEN)
      call crvn(6,geom2)


      call curveb(bsplin)
      call color(BLUE)
      call crvn(6,geom2)

c  Draw the four control points
      call color(YELLOW)
      call circf(geom2(1,1), geom2(2,1), 1.0)
      call circf(geom2(1,2), geom2(2,2), 1.0)
      call circf(geom2(1,3), geom2(2,3), 1.0)
      call circf(geom2(1,4), geom2(2,4), 1.0)
      call circf(geom2(1,5), geom2(2,5), 1.0)
      call circf(geom2(1,6), geom2(2,6), 1.0)
      call closeo

      call callob(1)

 999  continue
      dev = qtest()
      if (dev .ne. 0) then
	  dev = qread(val)
      endif
      if (dev .eq. ESCKEY) go to 99
      if (dev .eq. REDRAW) then
	  call reshap
	  call callob(1)
      end if
      go to 999

 99   continue

      call gexit
      stop
      end
