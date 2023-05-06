c
c   drawing.f:
c
c     This program is demonstrates implementation of various GL drawing 
c  commands.  The graphics window itself is forced to retain a 5 to 4 
c  aspect ratio with the window attribute call keepas[pect] which enables
c  the window to be redrawn relatively correctly if one sizes or resizes
c  the graphics window to be much smaller than the full console screen.
c     Press the "Esc"[ape] key to exit.
c

#include <gl/fgl.h>
#include <gl/fdevice.h>

      integer dev
      integer*2 val
      integer cone(2,3)
      integer xsize, ysize
      integer wid, i, j, strlen
      character*1 onech, string*50
      data cone /100,300,  150,100,  200,300/


      call keepas(5,4)
      call prefsi(1100,880)
      wid = winope('drawing example', 15)
      call qdevic(ESCKEY)

      call makeob(1)
      call getsiz(xsize, ysize)
      call viewpo(0,xsize-1,0,ysize-1)
      call ortho2(-0.5, xsize-0.5, -0.5, ysize-0.5)
c  draw an ice cream cone
      call color(white)
      call clear()
c  draw the cone
      call color(yellow)
      call polf2i(3,cone)
c  the first scoop is mint
      call color(green)
c  only half of it shows
      call arcfi(150,300,50,0,1800)
c  the second scoop is cherry
      call color(red)
      call circf(150.0,400.0,50.0)
      call color(black)
c  outline the cone in black
      call poly2i(3,cone)

c  next draw a few filled and unfilled arcs in the upper
c  left corner of the screen
      call arcf(100.0,650.0,40.0,450,2700)
      call arci(100,500,40,450,2700)
      call arcfi(250,650,80,2700,450)
      call arc(250.0,500.0,80.0,2700,450)

c  now, put up a series of filled and unfilled rectangles with
c  the names of their colors printed inside of them across the
c  rest of the top of the screen.
      call color(green)
      call recti(400,600,550,700)
      call cmov2i(420,640)
      call charst('green',5)

      call color(red)
      call rectfi(600,600,800,650)
      call color(black)
      call cmov2(690.0,620.0)
      call charst('red',3)
 
      call color(blue)
      call rect(810.0,700.0,1000.0,20.0)
      call cmov2i(900,300)
      call charst('blue',4)
 
c  now draw some text with a ruler on top to measure it by.
c  first the ruler:
      call color(black)
      call move2i(300,400)
      call draw2i(650,400)
      do 100 i=300,650,10
          call move2i(i,400)
          call draw2i(i,410)
100   continue

c  then some text:
      call cmov2i(300,380)
      string = 'the first line is drawn incorrectly '
      call charst(string,len(string))
      call charst('in two parts.',13)

c  note:  fortran pads string with spaces to its defined length,
c  and len(string) returns the defined length (50) instead of the
c  length of the character substring inserted into it.

      call cmov2i(300,364)
      string = 'this line is drawn correctly '
      strlen = len('this line is drawn correctly ')
      call charst(string,strlen)

c  this is the only way (other than counting by hand, as was
c  done for the second part of this line, below) of getting
c  the length of the actual set of characters to be printed.
      call charst('in two parts.',13)
 
      call cmov2i(300,352)
      strlen = len('this line is only 12 pixels lower.')
      call charst('this line is only 12 pixels lower.',strlen)
 
      call cmov2i(300,338)
      strlen = len('now move down 14 pixels ...')
      call charst('now move down 14 pixels ...',strlen)
 
      call cmov2i(300,322)
      strlen = len('and now down 16 ...')
      call charst('and now down 16 ...',strlen)
 
      call cmov2i(300,304)
      strlen = len('now 18 ...')
      call charst('now 18 ...',strlen)
 
      call cmov2i(300,284)
      strlen = len('and finally, 20 pixels.')
      call charst('and finally, 20 pixels.',strlen)
 
c  finally, show off the entire font.  the cmov2i() before each
c  character is necessary in case that character is not defined.
      do 300 i=0,3
          do 200 j=0,31
              call cmov2i(300 + 9*j, 200 - 18*i)
              onech(1:1) = char(32*i + j)
              call charst(onech,1)
200       continue
300   continue

      do 500 i=0,3
          call cmov2i(300, 100 - 18*i)
          do 400 j=0,31
              onech(1:1) = char(32*i + j)
              call charst(onech,1)
400       continue
500   continue
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
