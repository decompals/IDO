c
c   picking.f:
c
c      This program demonstrates picking using 3 explicit objects: a filled
c  and an outline circle, and a filled rectangle.  Pressing LEFTMOUSE at any
c  given point enters pick mode and tests to see if, at that pixel location,
c  there is any object detected, or else only the background.
c      Press the "Esc"[ape] key to exit.
c

#include <gl/fgl.h>
#include <gl/fdevice.h>

      integer*2 nambuf(300)
      integer*2 val, i, j, k, l
      integer   wid, dev, numpic, total
      integer   xsize, ysize

      xsize = 600
      ysize = YMAXSC-350
      call prefsi(xsize, ysize)
      wid = winope('pick',4)
      call winset(wid)

      call qdevic(ESCKEY)
      call qdevic(LEFTMO)
      call qdevic(RIGHTM)


C       make all objects
      call makeob(110)
      call pushna(110)
      call color(GREEN)
      call recti(164, 33, 364, 600)
      call popnam()
      call closeo()

      call makeob(120)
C        push 119 onto the name list stack. 
      call pushna(119)
C        push 120 (119 and 120) onto the name list stack. 
      call pushna(120)
      call color(CYAN)
      call cmov2i(400,400)
      call charst('Cyan',4)
C        remove 120 (119)
      call popnam
      call pushna(121)
      call color(YELLOW)
      call rectfi(400,100,500,300)
      call popnam
      call pushna(122)
      call color(BLACK)
      call cmov2i(420,200)
      call charst('Black',5)
      call popnam()      
      call pushna(123) 
      call color(WHITE)
      call rectfi(100, 100, 200, 200)
      call popnam()
      call popnam()
      call closeo()

C       100 -- THE MAIN OBJECT 
      call makeob(100)        
C       clear the name stack    
      call initna()      
      call loadna(100)
      call callob(110)
      call color(WHITE)
      call callob(120)
      call closeo()

      call ortho2(-0.5, xsize+0.5, -0.5, ysize-0.5)
      call color(BLUE)
      call clear
C        draw the object on the screen
      call callob(100)
      total = 0
      print *,' '

C       loop until the ESCKEY key is pushed
100   continue

      dev = qread(val)
C        try again if the event was a button release
      if (val .EQ. 0) go to 100
C        if the ESCKEY key is pushed, the program exits
      if (dev .EQ. ESCKEY) then
       	 print *, 'Total Hits:  ', total
	 go to 999
      end if

      if (dev .EQ. REDRAW) then
	  call reshap
          call ortho2(-0.5, xsize+0.5, -0.5, ysize-0.5)
	  call color(BLACK)
	  call clear
	  call callob(100)

C        if the LEFTMOUSE button is pushed, the IRIS enters pick mode
      else if (dev .EQ. LEFTMO) then
         call pushma 
         call pick(nambuf,300)
	 call initna
C           restate the projection transformation for the object
         call ortho2(-0.5, xsize+0.5 , -0.5, ysize+0.5)
C           call the object (no actual drawing takes place)
         call callob(100)

C           print out the number of hits and a name list for each hit
         numpic = endpic(nambuf)
	 call popmat          

	  print*, 'hits: ',numpic
	  j=1
	  if (numpic .NE. 0) then
	  do 300 i=1,numpic
		 k=nambuf(j)
		 j=j+1
		 print *,k
		 total=total+1
		 do 200 l=1,k
		   print *,' ',nambuf(j)
		   j=j+1
 200             continue
 300      continue
	  endif
      end if

C       loop until the ESCKEY key is pushed
      go to 100
999   continue

      call gexit
      stop
      end

