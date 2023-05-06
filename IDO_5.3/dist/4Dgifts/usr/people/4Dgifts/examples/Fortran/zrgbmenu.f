c
c    zrgbmenu.f:
c
c     This program implements a combination of various GL features:
c
c       doublebuffered-RGB mode zbuffering, with a pop-up menu.
c
c   It further implements--either manually via LEFTMOUSE, or in an
c   "automatic" (i.e. animation) mode format--movement of the polygons 
c   via compound rotations to allow for continuous screen-oriented 
c   rotations (see orient(), spin(), and draw_scene() below).  Horizontal
c   mouse movement rotates the polygons about the y-axis where right is 
c   positive and left is negative.  Vertical mouse movement rotates the
c   polygons about the x-axis where down is positive and up is negative.
c     Press the "Esc"[ape] key to exit.
c    Please note that this program will not work correctly on any 8-bit
c   IRIS machines--such minimal configurations do not contain the 
c   necessary number of bitplanes needed to perform accurate zbuffering.
c   The test immediately after the initialize routine in main() will
c   halt execution if it is discovered that the zbuffer option is not
c   installed by a FALSE returned valuie from getzbuffer().
c

#include <gl/fgl.h>
#include <gl/fdevice.h>


        common /matrix_stuff/ objmat(4,4), aidmat(4,4)
        common /mouse_stuff/  iomx, mx, iomy, my
        common /popup_stuff/  menu, MANUAL, IAUTOMATIC
        common /flags/        mode, NOTHING, IORIENT, ISPIN
        common /machzval/     izfar

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

        integer dev, menuval
        integer*2 val
        integer testifZinst


        mode = 0
        call initialize

        testifZinst = getzbu()
        if (testifZinst .eq. 0) then
	    print*, 'BUMmer!--zrgbmenu will not work on this machine'
	    print*, '        -no zbuffer option is installed.'
	    go to 999
        end if
        izfar = getgde(GDZMAX)

        call draw_scene

        do while (.true.)
 
            do while (qtest() .eq. 0 .and. mode .eq. ISPIN)
                call update_scene
            end do

            dev = qread(val)


            if (dev .eq. ESCKEY) then
                if (val .eq. 0) goto 999

            else if (dev .eq. REDRAW) then
                call reshap
                call draw_scene

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

        call gexit
	stop
        end


c==============================================================================
        subroutine initialize

#include <gl/fgl.h>
#include <gl/fdevice.h>

        common /popup_stuff/  menu, MANUAL, IAUTOMATIC

        call keepas (5, 4)
        igid = winope ('Zbuffered RGB #2', 16)

        call double
        call RGBmod
        call gconfi
        call zbuffe (.TRUE.)

        call qdevic (ESCKEY)
        call qdevic (RIGHTM)
        call qdevic (LEFTMO)
        call qdevic (MOUSEX)
        call qdevic (MOUSEY)

        menu = newpup()
        call addtop(menu,'Zbuffered-RGB #2 %t|Manual|Automatic',36,0)

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
        common /machzval/     izfar


        call czclea($00C86428, izfar)

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
        call cpack($00000000)
        call v3f(polygon1(1,1))
        call cpack($007F7F7F)
        call v3f(polygon1(1,2))
        call cpack($00FFFFFF)
        call v3f(polygon1(1,3))
        call endpol

        call bgnpol
        call cpack($0000FFFF)
        call v3f(polygon2(1,1))
        call cpack($007FFF00)
        call v3f(polygon2(1,2))
        call cpack($00FF0000)
        call v3f(polygon2(1,3))
        call endpol

        call bgnpol
        call cpack($0000FFFF)
        call v3f(polygon3(1,1))
        call cpack($00FF00FF)
        call v3f(polygon3(1,2))
        call cpack($00FF0000)
        call v3f(polygon3(1,3))
        call cpack($00FF00FF)
        call v3f(polygon3(1,4))
        call endpol

        return
        end


