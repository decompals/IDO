c  
c     depthcube.f:
c
c        This program draws a wire-framed cube with random points inside of
c   implementing depthcueing to indicate the parts of the cube closest to 
c   the viewer's eye.  
c        Experiment with the finer subtleties of depthcuing by changing
c   the values of perspective's NEAR and FAR Z clip-planes below, as well
c   as the eyepoint Z position (3rd parameter) of lookat.
c        Press the "Esc"[ape] key to exit.
c

#include <gl/fgl.h>
#include <gl/fdevice.h>

      integer   redchanl(32), grechanl(32), bluchanl(32)
      integer   i, j, n, cmc, once, dev
      integer*2 redval, greval, bluval
      integer*2 r, g, b, val
      integer*4 iznear, izfar
      real      hrand, ranval

      call foregr
      call keepas(1,1)
      wid = winope('dc',2)
      call double
      call gconfi
      call qdevic(ESCKEY)

c  to compare an orthogonal projection to one using perspective, comment
c  out the perspe/lookat lines, and uncomment the ortho line below
c     call ortho(-350.0,350.0,-350.0,350.0,-350.0,350.0)
      call perspe(470, 1.0, 350.0, 700.0)
      call lookat(0, 0.0, 700.0, 0.0, 0.0, 0.0, 0)

      call makeob(1)
      do i = 1, 100
         call pnt(hrand(-200.0,200.0),hrand(-200.0,200.0),
     %            hrand(-200.0,200.0))
      end do

      call movei(-200,-200,-200)
      call drawi( 200,-200,-200)
      call drawi( 200, 200,-200)
      call drawi(-200, 200,-200)
      call drawi(-200,-200,-200)
      call drawi(-200,-200, 200)
      call drawi(-200, 200, 200)
      call drawi(-200, 200,-200)
      call movei(-200, 200, 200)
      call drawi( 200, 200, 200)
      call drawi( 200,-200, 200)
      call drawi(-200,-200, 200)
      call movei( 200, 200, 200)
      call drawi( 200, 200,-200)
      call movei( 200,-200,-200)
      call drawi( 200,-200, 200)
      call closeo

c  get the original color values from 128 up to 255 and save them into
c  their three respective arrays
      j = 1
      do i = 128, 255
	 call getmco(i, redval, greval, bluval)
	 redchanl(j) = redval
	 grechanl(j) = greval
	 bluchanl(j) = bluval
	 j = j + 1
      end do

C  Load the color map with a cyan ramp.
      do i = 0, 127
         call mapcol(128+i, 0 , 2*i ,2*i)
      end do
      once = 1

C  Set up the mapping of z values to color map indices:
C  znear is mapped to index 128 and zfar is mapped to index 255
      call glcomp(GLCZRA, 0)
      iznear = getgde(GDZMIN)
      izfar  = getgde(GDZMAX)
      call lshade(128, 255, iznear, izfar)

C  Turn on depthcue mode: the color index of each pixel in points and 
C  lines is determined from the z value of the pixel
      call depthc(.TRUE.)

C  Until a key is pressed, rotate the cube according to the 
C  movement of the mouse
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
      call pushma
      call rotate(3*getval(MOUSEY),'x')
      call rotate(3*getval(MOUSEX),'y')
      call color(BLACK)
      call clear
      call callob(1)
      call popmat
      call swapbu
      go to 25

 99   continue
c  now that we are finished, restore the original color index values
      j = 1
      do i = 128, 255
	 call mapcol(i, redchanl(j), grechanl(j), bluchanl(j))
	 j = j + 1
      end do

      call gexit
      stop
      end

      function hrand(low,high)
      real low,high
      real hrand
      real*8 rand

      hrand= (rand()*(high-low))+low
      return
      end
