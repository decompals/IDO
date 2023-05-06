c 
c    zcmapmenu.c:
c 
c      This program implements a combination of various GL features:
c 
c        doublebuffered color-map mode zbuffering, with a pop-up menu.
c 
c    It further implements--either manually via LEFTMOUSE, or in an
c    "automatic" (i.e. animation) mode format--movement of the polygons 
c    via compound rotations to allow for continuous screen-oriented 
c    rotations (see orient(), spin(), and draw_scene() below).  Horizontal
c    mouse movement rotates the polygons about the y-axis where right is 
c    positive and left is negative.  Vertical mouse movement rotates the
c    polygons about the x-axis where down is positive and up is negative.
c 
c    This program also demonstrates a standard approach to writing code
c    that does not eat up CPU cycles if the mouse is elsewhere than over
c    this window or if the mouse isn't moving.  (Past examples have included 
c    the very expensive qtest() or getbutton() cycling that can cost alot of 
c    CPU time).
c 
c    This is accomplished by catching INPUTCHANGE, MOUSEX, and MOUSEY events
c    in the infinite loop in main().  When INPUTCHANGE is the device dumped 
c    into the queue, and its data returned in the variable val is zero, this 
c    means the input focus (i.e. the user's mouse) has left the confines of 
c    this graphics window.  When val equals one, this means the input focus
c    has re-attached itself to this program, so at this time "mode" gets reas-
c    signed with its' previous state.  Notice that once the user moves the 
c    mouse outside of this window, control flow will come to rest on the 
c    second statement in the MAIN infinite loop, dev = qread(&val), and 
c    will block on this until the user brings the mouse back into the 
c    window.  In this way, CPU usage is effectively halted for this 
c    program.  This way of "turning off" an inactive program--provided one 
c    is NOT interested in maintaining the automatic mode animation (where 
c    polling or qtest() -ing would be necessary to determine when the 
c    inputfocus had returned)--so that it does not eat up extra CPU cycles 
c    when it is not actually in use, is the recommended model for optimal 
c    use of graphics on the IRIS.
c 
c    Another thing that happens when the input focus is returned to the
c    window is that the colormap gets rebuilt for this application.  That way,
c    if many programs need to do things with the colormap, the one that has
c    the input focus is the one that has last built its colormap.
c 
c    When the program is in manual mode, it uses the MOUSEX and MOUSEY 
c    events to determine when to update the object's compound matrix and
c    redraw the scene.  This way, the CPU doesn't waste time checking
c    if something has happened, and the program only updates the scene if 
c    the mouse has moved.
c 
c    Also, usage of the device WINQUIT is employed here to enable the 
c    program catching a "Quit" signal that can come either from the NeWS 
c    window-border menu, or by "boinking" on the lightning bolt on the 
c    right side of the window's title bar.  This is extremely useful in the 
c    case of this program as it can recognize--even in these cases--that 
c    "its time to go", and can first restore the portion of the color-map 
c    that it changed back to its previous values.  One can also press the 
c    "Esc"[ape] key to exit.
c 
c    Please note that this program will not work correctly on any 8-bit
c    IRIS machines--such minimal configurations do not contain the 
c    necessary number of bitplanes needed to perform accurate zbuffering.
c    The test immediately after the initialize routine in main() will
c    halt execution if it is discovered that the zbuffer option is not
c    installed by a FALSE returned valuie from getzbuffer().
c

#include <gl/fgl.h>
#include <gl/fdevice.h>


        common /matrix_stuff/   objmat(4,4), aidmat(4,4)
        common /mouse_stuff/    iomx, mx, iomy, my
        common /popup_stuff/    menu, MANUAL, IAUTOMATIC
        common /flags/          mode, NOTHING, IORIENT, ISPIN
        common /colormap_stuff/ iredchnl(97), igrechnl(97), ibluchnl(97)
	common /color_index_stuff/ IBACKGROUND, ISTARTINDEX, IENDINDEX
        common /machzval/       izfar

        data objmat /1.0, 0.0, 0.0, 0.0,
     &               0.0, 1.0, 0.0, 0.0,
     &               0.0, 0.0, 1.0, 0.0,
     &               0.0, 0.0, 0.0, 1.0/

        data aidmat /1.0, 0.0, 0.0, 0.0,
     &               0.0, 1.0, 0.0, 0.0,
     &               0.0, 0.0, 1.0, 0.0,
     &               0.0, 0.0, 0.0, 1.0/

 
        data NOTHING    /0/
        data IORIENT    /1/
        data ISPIN      /2/

        data MANUAL     /1/
        data IAUTOMATIC /2/

	data ISTARTINDEX  /640/
	data IENDINDEX    /735/
	data IBACKGROUND /736/

        integer*2 val
	integer dev
        integer i, j
	integer menuval
	integer lastmode
        integer testifZinst


        mode = 0
        call initialize

        testifZinst = getzbu()
        if (testifZinst .eq. 0) then
	    print*, 'BUMmer!--zcmapmenu will not work on this machine'
	    print*, '        -no zbuffer option is installed.'
	    go to 888
        end if
        izfar = getgde(GDZMAX)

        call draw_scene

        do while (.true.)
 
            do while (qtest() .eq. 0 .and. mode .eq. ISPIN)
                call update_scene
            end do

            dev = qread(val)

c  Exit when key is going up, not down.  This avoids the scenario 
c  where a window underneath this program's window--say a wsh--would 
c  otherwise "eat up" the up-event of the Esc key being released. 
            if (dev .eq. ESCKEY) then
                if (val .eq. 0) goto 999

c  Regarding the usage of WINQUIT, which will be put into the queue 
c  upon "boinking" on the lighting bolt or by choosing the "Quit" 
c  NeWS menu item: notice how it DOES NOT test to see if see 
c  "if (val .eq. 0)" is true because it returns the graphics ID of 
c  the window being "quitted" and thus will always be non-zero.
            else if (dev .eq. WINQUI) then
		goto 999

            else if (dev .eq. REDRAW) then
                call reshap
                call draw_scene

c  testing for INPUTCHANGE is explained up in the beginning comments
            else if (dev .eq. INPTCH) then
                if (val .ne. 0) then
		    mode = lastmode
		    call buildmap
                else
		    lastmode = mode
		    mode = NOTHING
		end if

            else if (dev .eq. LEFTMO) then
                iomx = mx
                iomy = my
                if (val .ne. 0) then
                    mode = IORIENT
                else
                    mode = NOTHING
                end if

            else if (dev .eq. RIGHTM) then
                if (val .ne. 0) then
                    menuval = dopup(menu)
                    if (menuval .eq. MANUAL) then
                        mode = NOTHING
                    else if (menuval .eq. IAUTOMATIC) then
                        mode = ISPIN
                    end if 
                end if


            else if (dev .eq. MOUSEX) then
                    iomx = mx
                    mx   = val
                    call update_scene

            else if (dev .eq. MOUSEY) then
                    iomy = my
                    my   = val
                    call update_scene

            end if


        end do

999     continue

c  now that we are finished, restore the original color index values
        j = 1
        do i = ISTARTINDEX, IBACKGROUND
            call mapcol(i, iredchnl(j), igrechnl(j), ibluchnl(j))
            j = j + 1
        end do

888     continue

        call gexit
        stop
        end


c==============================================================================
        subroutine initialize

#include <gl/fgl.h>
#include <gl/fdevice.h>

        common /popup_stuff/  menu, MANUAL, IAUTOMATIC
        common /colormap_stuff/ iredchnl(97), igrechnl(97), ibluchnl(97)
	common /color_index_stuff/ IBACKGROUND, ISTARTINDEX, IENDINDEX

        integer i, j
        integer*2 redval, greval, bluval
	integer*2 i1, i2, i3, i4, i5, i6, i7, i8

        call keepas (5, 4)
	call foregr
        igid = winope ('Zbuffered Color-Map #3', 22)

        call double
        call gconfi
        call zbuffe (.TRUE.)

        call qdevic (ESCKEY)
        call qdevic (RIGHTM)
        call qdevic (LEFTMO)
        call qdevic (MOUSEX)
        call qdevic (MOUSEY)
        call qdevic (WINQUI)
        call qdevic (WINSHU)

        menu = newpup()
        call addtop(menu,'Zbuferd Colr-Map #3 %t|Manual|Automatic',40,0)

c  first save out the original color map values between 32 and 127 
        j = 1
        do i = ISTARTINDEX, IBACKGROUND
            call getmco(i, redval, greval, bluval)
            iredchnl(j) = redval
            igrechnl(j) = greval
            ibluchnl(j) = bluval
            j = j + 1
        end do

	call buildmap

        return
	end


c==============================================================================
	subroutine buildmap

#include <gl/fgl.h>

	common /color_index_stuff/ IBACKGROUND, ISTARTINDEX, IENDINDEX

	integer*2 i1, i2, i3, i4, i5, i6, i7, i8


        call mapcol(IBACKGROUND, 40, 100, 200)

c  now build out 3 color ramps of 32 shades that make ranges of:
c    32-63:   YELLOW to BLUE
c    64-95:   CYAN   to RED
c    96-127:  GREEN  to MAGENTA
        i1 = ISTARTINDEX
	i2 = ISTARTINDEX+31
	i3 = 255
	i4 =   0
	i5 = 255
	i6 =   0 
	i7 =   0
	i8 = 255
        call rampup(i1, i2, i3, i4, i5, i6, i7, i8) 
	i1 = ISTARTINDEX+32
	i2 = ISTARTINDEX+63
	i3 =   0 
	i4 = 255
	i5 =   0
	i6 = 255
	i7 =   0
	i8 = 255
        call rampup(i1, i2, i3, i4, i5, i6, i7, i8)
	i1 = ISTARTINDEX+64
	i2 = IENDINDEX
	i3 =   0
	i4 = 255
	i5 =   0
	i6 =   0
	i7 =   0
	i8 = 255
        call rampup(i1, i2, i3, i4, i5, i6, i7, i8)

        return
	end


c==============================================================================
c   rampup:
c     make a bi-linear interpolated color ramp taking as input the 
c     starting and ending look-up table indices, and the minimum 
c     and maximum values for red, green, and blue
c 
        subroutine rampup(start,end,minR,maxR,minG,maxG,minB,maxB)
        integer*2  start, end, minR, maxR, minG, maxG, minB, maxB   

        integer*2 len_red, len_green, len_blue                     
        integer   red, gre, blu, i
        real      rdx, gdx, bdx  
        real      r, g, b
        real      steps        
                       

        steps = end-start + 1

        len_red   = (maxR - minR) + 1
        len_green = (maxG - minG) + 1
        len_blue  = (maxB - minB) + 1

        rdx = len_red   / steps
        gdx = len_green / steps
        bdx = len_blue  / steps

        r = minR
        g = minG
        b = minB

        do i = start, end
            red = r + 0.5
            gre = g + 0.5
            blu = b + 0.5
            call mapcol(i, red, gre, blu)
            r = r + rdx
            g = g + gdx
            b = b + bdx
        end do

        return
        end


c==============================================================================
        subroutine update_scene

#include <gl/fgl.h>

        common /popup_stuff/  menu, MANUAL, IAUTOMATIC
        common /flags/        mode, NOTHING, IORIENT, ISPIN


        if (mode .eq. IORIENT) then
            call orient
        else if (mode .eq. IAUTOMATIC) then
            call spin
        endif

        if (mode .ne. 0) call draw_scene

        return
        end


c==============================================================================
        subroutine orient

#include <gl/fgl.h>

        common /matrix_stuff/ objmat(4,4), aidmat(4,4)
        common /mouse_stuff/  iomx, mx, iomy, my


        call pushma
        call loadma (aidmat)

        call rotate (mx-iomx, 'y')
        call rotate (iomy-my, 'x')

        call multma (objmat)
        call getmat (objmat)

        call popmat

        return
        end


c==============================================================================
        subroutine spin

#include <gl/fgl.h>

        common /matrix_stuff/ objmat(4,4), aidmat(4,4)
        common /mouse_stuff/  iomx, mx, iomy, my


        call pushma
        call loadma (aidmat)

        call rotate (5, 'x')
        call rotate (6, 'y')
        call rotate (4, 'y')

        call multma (objmat)
        call getmat (objmat)

        call popmat

        return
        end


c==============================================================================
        subroutine draw_scene

#include <gl/fgl.h>

        common /matrix_stuff/ objmat(4,4), aidmat(4,4)
        common /colormap_stuff/ iredchnl(97), igrechnl(97), ibluchnl(97)
	common /color_index_stuff/ IBACKGROUND, ISTARTINDEX, IENDINDEX
        common /machzval/       izfar

        call czclea(IBACKGROUND, izfar)

        call perspe (400, 1.25, 30.0, 60.0)
        call transl (0.0, 0.0, -40.0)
        call multma (objmat)

c  to skew original view so all things are more visible initially
        call rotate (-580, 'y')
        call drawpolys

        call swapbu

        return
        end


c==============================================================================
        subroutine drawpolys

#include <gl/fgl.h>

	common /color_index_stuff/ IBACKGROUND, ISTARTINDEX, IENDINDEX

        dimension polygon1(3,3)
        dimension polygon2(3,3)
        dimension polygon3(3,4)

        data polygon1 /-10.0, -10.0,   0.0,
     &                  10.0, -10.0,   0.0,
     &                 -10.0,  10.0,   0.0/
        data polygon2 /  0.0, -10.0, -10.0,
     &                   0.0, -10.0,  10.0,
     &                   0.0,   5.0, -10.0/
        data polygon3 /-10.0,   6.0,   4.0,
     &                 -10.0,   3.0,   4.0,
     &                   4.0,  -9.0, -10.0,
     &                   4.0,  -6.0, -10.0/

        call bgnpol
        call color(ISTARTINDEX+32)
        call v3f(polygon1(1,1))
        call color(ISTARTINDEX+63)
        call v3f(polygon1(1,2))
        call color(ISTARTINDEX+48)
        call v3f(polygon1(1,3))
        call endpol

        call bgnpol
        call color(IENDINDEX)
        call v3f(polygon2(1,1))
        call color(ISTARTINDEX+80)
        call v3f(polygon2(1,2))
        call color(ISTARTINDEX+64)
        call v3f(polygon2(1,3))
        call endpol

        call bgnpol
        call color(ISTARTINDEX)
        call v3f(polygon3(1,1))
        call color(ISTARTINDEX+16)
        call v3f(polygon3(1,2))
        call color(ISTARTINDEX+31)
        call v3f(polygon3(1,3))
        call color(ISTARTINDEX+16)
        call v3f(polygon3(1,4))
        call endpol

        return
        end
