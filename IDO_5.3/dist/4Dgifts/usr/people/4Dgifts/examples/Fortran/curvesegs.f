c
c    curvesegs.f
c
c      This program illustrates the basic usage of defining and drawing
c  the three types of curves available in the GL:  bezier (drawn in red),
c  cardinal (drawn in blue), and b-spline, (drawn in green).  The control
c  points used to define the three curves are drawn in yellow.
c      Press the "Esc"[ape] key to exit.
c

#include <gl/fgl.h>
#include <gl/fdevice.h>

      integer winid, dev
      integer bezier, cardin, bsplin
      real    asixth, msixth, tthrds
      real    bezmat(4,4), carmat(4,4), bspmat(4,4), geom1(3,4)
      parameter ( bezier = 1 )
      parameter ( cardin = 2 )
      parameter ( bsplin = 3 )
      parameter ( asixth = 1.0/6.0 )
      parameter ( msixth = -1.0/6.0 )
      parameter ( tthrds = 2.0/3.0 )

      data bezmat /-1.0,  3.0, -3.0,  1.0,
     2              3.0, -6.0,  3.0,  0.0,
     3             -3.0,  3.0,  0.0,  0.0,
     4              1.0,  0.0,  0.0,  0.0/

      data carmat /-0.5,  1.5, -1.5,  0.5,
     2              1.0, -2.5,  2.0, -0.5,
     3             -0.5,  0.0,  0.5,  0.0,
     4              0.0,  1.0,  0.0,  0.0/

      data bspmat /msixth,    0.5,   -0.5, asixth,
     2                0.5,   -1.0,    0.5,    0.0,
     3               -0.5,    0.0,    0.5,    0.0,
     4             asixth, tthrds, asixth,    0.0/
     
      data geom1 /100.0, 100.0, 0.0,
     2            200.0, 200.0, 0.0,
     3            200.0,   0.0, 0.0,
     4            300.0, 100.0, 0.0/

      call keepas(1,1)
      winid = winope("curve segments", 14)
      call qdevic(ESCKEY)

      call ortho(50.0, 350.0, -50.0, 250.0, -1.0, 1.0)

      call makeob(1)
      call color(BLACK)
      call clear
c  Define a basis matrix called bezier.
      call defbas(bezier,bezmat)
C  Identify the bezier matrix as the current basis matrix.
      call curveb(bezier)
C  The curve segment will be drawn using 20 line segments.
      call curvep(20)
      call color(RED)
c  Draw the curve based on the four control points in geom1.
      call crv(geom1)

c  Define a new basis.
      call defbas(cardin,carmat)
c  Reset the current basis.
      call curveb(cardin)
      call color(BLUE)
c  Draw a new curve segment.
c  Note: the curve precision does not have to be restated.
      call crv(geom1)

c  Define a new basis.
      call defbas(bsplin,bspmat)
c  Reset the current basis.
      call curveb(bsplin)
      call color(GREEN)
c  Draw a new curve segment.
      call crv(geom1)

c  Draw in the four control points
      call color(YELLOW)
      call circf(geom1(1,1), geom1(2,1), 1.0)
      call circf(geom1(1,2), geom1(2,2), 1.0)
      call circf(geom1(1,3), geom1(2,3), 1.0)
      call circf(geom1(1,4), geom1(2,4), 1.0)
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
      endif
      go to 999

 99   continue
      call gexit
      stop
      end
