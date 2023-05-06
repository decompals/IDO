c
c    surfpatch.f:
c
c      This program demostrates drawing three surface patches, each with
c  the same sixteen control points, the same number of curve segments, the
c  same precisions, but each with a different basis matrix:  RED is bezier,
c  GREEN is cardinal, and BLUE is B-spline.
c      Press the "Esc"[ape] key to exit.
c

#include <gl/fgl.h>
#include <gl/fdevice.h>

      integer dev
      integer*2 val
      integer xsize, ysize
      integer bezier, cardin, bsplin
      real bezmat(4,4), carmat(4,4), bspmat(4,4)
      real geomx(4,4), geomy(4,4), geomz(4,4)
      real asixth, msixth, tthrds
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
      data geomx /  0.0, 100.0, 200.0, 300.0,
     2              0.0, 100.0, 200.0, 300.0,
     3            700.0, 600.0, 500.0, 400.0,
     4            700.0, 600.0, 500.0, 400.0/
      data geomy /400.0, 500.0, 600.0, 700.0,
     2              0.0, 100.0, 200.0, 300.0,
     3              0.0, 100.0, 200.0, 300.0,
     4            400.0, 500.0, 600.0, 700.0/
      data geomz /100.0, 200.0, 300.0, 400.0,
     2            100.0, 200.0, 300.0, 400.0,
     3            100.0, 200.0, 300.0, 400.0,
     4            100.0, 200.0, 300.0, 400.0/

      xsize = 750
      ysize = 750
      call prefsi(xsize,ysize)
      winid = winope('surfpatch', 9)
      call qdevic(ESCKEY)

      call makeob(1)
      call color(BLACK)
      call clear()
      call viewpo(0,xsize-1,0,ysize-1)
      call ortho(-0.5,xsize-0.5,-0.5,ysize-0.5,xsize-0.5,-(xsize-0.5))
      call transl(25.0, 0.0, 0.0)
c   define a basis matrix called bezier
      call defbas(bezier,bezmat)
c   define a basis matrix called cardin
      call defbas(cardin,carmat)
c   define a basis matrix called bsplin
      call defbas(bsplin,bspmat)

c       a bezier basis will be used for both directions in the first patch
      call patchb(bezier,bezier)
c   7 curve segments will be drawn in the u direction and 4 in the v direction
      call patchc(4,7)
c   the curve segments in the u direction will consist of 20 line
c   segments (the lowest multiple of vcurves greater than usesegments)
c   & the curve segments in the v direction will consist of 21 line
c   segments (the lowest multiple of ucurves greater than vsegments)
      call patchp(20,20)
      call color(RED)
c   the patch is drawn based on the 16 specified control points    
      call patch(geomx,geomy,geomz)

c       reset the bases for both directions
      call patchb(cardin,cardin)
      call color(GREEN)
c       draw another patch using the same control points but a different basis
      call patch(geomx,geomy,geomz)

c       reset the bases for both directions again
      call patchb(bsplin,bsplin)
      call color(BLUE)
c       draw a third patch
      call patch(geomx,geomy,geomz)
      call closeo

      call callob(1)

 25   continue
      dev = qtest()
      if (dev .ne. 0) then
          dev = qread(val)
      endif
      if (dev .eq. ESCKEY) go to 99 
      if (dev .eq. REDRAW) then
	   call reshap 
	   call callob(1)
      endif
      go to 25
 99   continue

      call gexit()
      stop
      end
