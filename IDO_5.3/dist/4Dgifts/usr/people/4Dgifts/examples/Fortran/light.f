c
c
      program light
c
c
c     Example program illustrating GL lighting in fortran.  Program shows a
c       water molecule in a ball and stick model, with the oxygen atom being
c       red, and the hydorgen atoms being white.  Rotation of the molecule is
c       done with the '2', '4', '6' and '8' keys of the numeric keypad, and
c       zooming is done with the 'x' and 'z' keys.  
c
c                                                 Dave Shreiner
c                                                   4 May 1991
c

#     include "/usr/include/gl/fgl.h"
#     include "/usr/include/gl/fdevice.h"

      integer*2 val
      integer*4 dev

      integer*4 dang
      parameter(dang = 25)

      real sphere(3,40,40), dist, aspect, near, far
      integer azim, inc, fovy, minfov, maxfov
      common sphere, dist, azim, inc, fovy, aspect, near, far

      data aspect, near, far / 1.0, 1.0, 9.0 /
      data minfov, maxfov / 20, 1780 /

      call init

c
c *** User input processing loop
c

 10   dev = qread(val)

      if (val .eq. 1) then
         if (dev .eq. ESCKEY) call exit
         if (dev .eq. PAD2) inc = inc - dang
         if (dev .eq. PAD4) azim = azim - dang
         if (dev .eq. PAD6) azim = azim + dang
         if (dev .eq. PAD8) inc = inc + dang
         if (dev .eq. REDRAW) call reshap
c
c *** Control zooming in with the 'x' key.  For this example, we will use the
c       field of view in the prespe to zoom in and out.  This illustrates what
c       is required to take care of the multi-matrix mode.
c

         if (dev .eq. XKEY) then
            fovy = fovy - dang
            if (fovy .lt. minfov)   fovy = minfov
c
c *** Set the current stack to be update to the PROJECTION stack.  This is a
c       one matrix stack with only either an orthogonal [ortho()] or 
c       perspective [perspe()] matrix on it.  Every time a new call to ortho()
c       or perspe() is make, the previous matrix is discarded.  This is the
c       case even in single matrix mode.
c
            call mmode(MPROJE)
            call perspe(fovy, aspect, near, far)
         endif

c
c *** Repeat the same for zooming out with the 'z' key.
c

         if (dev .eq. ZKEY) then
            fovy = fovy + dang
            if (fovy .gt. maxfov)   fovy = maxfov
            call mmode(MPROJE)
            call perspe(fovy, aspect, near, far)
         endif
      endif

      if (qtest() .ne. 0) go to 10

      call atoms

      go to 10

      end

c
c ---------------------------------------------------------------------------
c
      subroutine init
c
c     This subroutine sets up and executes all the required GL calls.
c

#     include "/usr/include/gl/fgl.h"
#     include "/usr/include/gl/fdevice.h"

      integer*4 matsiz, ligsiz, modsiz, width, height
      parameter (matsiz = 21, ligsiz = 14, modsiz = 10)

      real pi
      parameter (pi = 3.1415)

      real oxygen(matsiz), hydrog(matsiz), light(ligsiz), model(modsiz)

      real sphere(3,40,40), dist, aspect, near, far
      integer inc, azim, fovy

      common sphere, dist, azim, inc, fovy, aspect, near, far

c
c *** Initalize data arrays to be passed to initalize lighting properties.
c

      data oxygen /
     *   AMBIEN, 0.25, 0.0, 0.0,
     *   DIFFUS, 0.5, 0.0, 0.0,
     *   EMISSI, 0.0, 0.0, 0.0,
     *   SPECUL, 1.0, 0.0, 0.0,
     *   SHININ, 64.0,
     *   ALPHA, 0.0,
     *   LMNULL /

      data hydrog /
     *   AMBIEN, 0.25, 0.25, 0.25,
     *   DIFFUS, 0.5, 0.5, 0.5,
     *   EMISSI, 0.0, 0.0, 0.0,
     *   SPECUL, 1.0, 1.0, 1.0,
     *   SHININ, 64.0,
     *   ALPHA, 0.0,
     *   LMNULL /

      data light /
     *   AMBIEN, 0.3, 0.3, 0.3,
     *   LCOLOR, 1.0, 1.0, 1.0,
     *   POSITI, 1.0, 1.0, 1.0, 0.0,
     *   LMNULL /

      data model /
     *   AMBIEN, 0.3, 0.3, 0.3,
     *   ATTENU, 1.0, 0.0,
     *   LOCALV, 0.0,
     *   LMNULL /

      inc = 0
      azim = 0
      dist = 5.0

c
c *** Set up and open window, and configure graphics the way we want.
c

      width = getgde(GDXPMA) / 4
      height = getgde(GDYPMA) / 4

      call prefpo(width, 3*width, height, 3*height)
      call winope('Lighting Example', 16)

      call rgbmod
      call double
      call gconfi

      call zbuffe(.true.)

      call qdevic(ESCKEY)
      call qdevic(XKEY)
      call qdevic(ZKEY)
      call qdevic(PAD2)
      call qdevic(PAD4)
      call qdevic(PAD6)
      call qdevic(PAD8)

c
c *** Start multi-matrix mode.  The following two calls set up the perspective
c       that will be used initially.  Zooming is done by changing the field of
c       view angle (parameter one of the perspe() call).  See above.
c

      fovy = 600
      aspect = 1.0
      near = 1.0
      far = 9.0

      call mmode(MPROJE)
      call perspe(fovy, aspect, near, far)

c
c *** Define all the necessary lighting properties : materials, lights, and
c        models.  See lmdef for more detail.  The calls to lmdef(), and 
c        lmbind, must follow the first call to mmode(MPROJE).  The call to
c        mmode(MPROJE) lets the system know that we want to do lighting, and
c        will probably be passing along property definitions and the like.
c

      call lmdef(DEFMAT, 1, matsiz ,oxygen)
      call lmdef(DEFMAT, 2, matsiz, hydrog)
      call lmdef(DEFLIG, 1, ligsiz, light)
      call lmdef(DEFLMO, 1, modsiz, model)

c
c *** Set the current light and lighting model.  If you were to have more than
c       one light in the scene at one time, you would use LIGHT1, LIGHT2, ...
c       also in calls to lmbind.  There is only one model allowed at one time,
c       so if you had multiple models defined, you would switch between models
c       with the lmbind(LMODEL, ...) call.
c
      call lmbind(LIGHT0, 1)
      call lmbind(LMODEL, 1)

c
c *** Define the points, and also the normals, for the sphere.  Since a sphere
c       has the property that the normal at a point is equal the the coords.
c       of that point, we only have to calculate the points, and get the 
c       normals for free.  Unfortunately, this is, in general, not possible,
c       and normals need also be computed.
c
      dth = 2*pi/39.0
      dphi = 2*pi/39.0

      th = 0.00
      phi = 0.00

      do i = 1, 40
         do j = 1, 40
            sphere(1,i,j) = sin(phi)*cos(th)
            sphere(2,i,j) = sin(phi)*sin(th)
            sphere(3,i,j) = cos(phi)
            th = th + dth
         end do
         phi = phi + dphi
      end do

      return
      end
c
c ---------------------------------------------------------------------------
c
      subroutine atoms
c
c     This subroutine draws the molecule with lighting applied.
c

#     include "/usr/include/gl/fgl.h"

      integer c(3), l(3)
      real p(3)
      data c / 0, 0, 0 /
      data l / 255, 255, 255 /

      real sphere(3,40,40), dist, aspect, near, far
      integer azim, inc, fovy
      common sphere, dist, azim, inc, fovy, aspect, near, far

c
c *** Clear the background and z-buffer planes.
c

      call c3i(c)
      call clear
      call zclear

c
c *** Set up the viewing transformation (in the instance, polarview).  We need
c       to call mmode(MVIEWI) to switch back to the viewing matrix stack from
c       the perspective matrix stack.
c

      call mmode(MVIEWI)
      call pushma
      call polarv(dist, azim, inc, 0)

c
c *** Draw the atomic bonds as white lines.
c

      call c3i(l)

      p(1) = 0.00
      p(2) = 0.00
      p(3) = 0.00

      call bgnlin
      call v3f(p)
      p(1) = -2.00
      p(2) = -2.00
      p(3) = -2.00
      call v3f(p)
      call endlin

      p(1) = 0.00
      p(2) = 0.00
      p(3) = 0.00

      call bgnlin
      call v3f(p)
      p(1) = 2.00
      p(2) = 2.00
      p(3) = -2.00
      call v3f(p)
      call endlin

c
c *** Bind the material at index 1 (the definition for oxygen) to be the
c       current material.  All polygonal graphics from this point until a new
c       material is bound use these material properties.
c

      call lmbind(MATERI, 1)
      call drwsph

c
c *** Change the material properties to reflect the hydrogen atoms definiton.
c

      call lmbind(MATERI, 2)

      call pushma
      call transl(-2.0, -2.0, -2.0)
      call scale(0.65, 0.65, 0.65)
      call drwsph
      call popmat

      call pushma
      call transl(2.0, 2.0, -2.0)
      call scale(0.65, 0.65, 0.65)
      call drwsph
      call popmat

      call popmat

      call swapbu

      return
      end
c
c ---------------------------------------------------------------------------
c
      subroutine drwsph
c
c     This subroutine draws a single sphere with radius 1.0, centered at the
c       origin.  In addition, all vertices have a normal associated with them.
c       There are two tmesh type polygons drawn inside the loop.  This is to
c       make sure that the polygons are presented in a anit-clockwise manner 
c       to make sure that backface removal work correctly.
c

#     include "/usr/include/gl/fgl.h"

      real sphere(3,40,40), dist, aspect, near, far
      integer azim, inc, fovy
      common sphere, dist, azim, inc, fovy, aspect, near, far

      do i = 1,39
         do j = 1,39
            call bgntme
c
c *** Define a normal for a vertex with a call to n3f().  For a sphere, the
c        vector for the normal is identical to the vector describing the
c        point.  That's why we can pass the same data to the n3f() call as 
c        we do for the v3f() call.  Also, note that the points are given
c        in an anti-clockwise manner to keep the backface removal information
c        correct.
c
            call n3f(sphere(1,i,j))
            call v3f(sphere(1,i,j))
            call n3f(sphere(1,i,j+1))
            call v3f(sphere(1,i,j+1))
            call n3f(sphere(1,i+1,j))
            call v3f(sphere(1,i+1,j))
            call endtme

            call bgntme
            call n3f(sphere(1,i+1,j))
            call v3f(sphere(1,i+1,j))
            call n3f(sphere(1,i,j+1))
            call v3f(sphere(1,i,j+1))
            call n3f(sphere(1,i+1,j+1))
            call v3f(sphere(1,i+1,j+1))
            call endtme
         end do
      end do

      return
      end
