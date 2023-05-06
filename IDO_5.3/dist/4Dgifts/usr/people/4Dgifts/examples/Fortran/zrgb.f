c
c    zrgb.f
c
c     This program demostrates zbuffering in RGB mode combined with the 
c  ability to perform compound rotations via mouse movement of three inter-
c  secting polygons to allow for continuous screen-oriented rotations.  
c  Horizontal mouse movement rotates the polygons about the y-axis where 
c  right is positive and left is negative.  Vertical mouse movement rotates 
c  the polygons about the x-axis where down is positive and up is negative.
c     Press the "Esc"[ape] key to exit.
c     Please note that this program will not work correctly on any 8-bit
c  IRIS machines--such minimal configurations do not contain the 
c  necessary number of bitplanes needed to perform accurate zbuffering.
c  The test immediately after the initialize routine in main() will
c  halt execution if it is discovered that the zbuffer option is not
c  installed by a FALSE returned valuie from getzbuffer().
c

#include <gl/fgl.h>
#include <gl/fdevice.h>

        common /matrix_stuff/ objmat(4,4), aidmat(4,4)
        common /mouse_stuff/  iomx, mx, iomy, my
        common /flags/        mode, NOTHING, IORIENT
        common /machzval/     izfar

        data objmat /1.0, 0.0, 0.0, 0.0,
     &               0.0, 1.0, 0.0, 0.0,
     &               0.0, 0.0, 1.0, 0.0,
     &               0.0, 0.0, 0.0, 1.0/

        data aidmat /1.0, 0.0, 0.0, 0.0,
     &               0.0, 1.0, 0.0, 0.0,
     &               0.0, 0.0, 1.0, 0.0,
     &               0.0, 0.0, 0.0, 1.0/

        data NOTHING /0/
        data IORIENT /1/

	integer dev
        integer*2 val
        integer testifZinst


        call initialize

        testifZinst = getzbu()
        if (testifZinst .eq. 0) then
	    print*, 'BUMmer!--zrgb will not work on this machine'
	    print*, '        -no zbuffer option is installed.'
	    go to 999
        end if
        izfar = getgde(GDZMAX)

        call draw_scene

        do while (.true.)

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

        call keepas (5, 4)
        igid = winope ('Zbuffered RGB #1', 16)

        call double
        call RGBmod
        call gconfi
        call zbuffe (.TRUE.)

        call qdevic (ESCKEY)
        call qdevic (LEFTMO)
        call qdevic (MOUSEX)
        call qdevic (MOUSEY)

        return
        end


c==============================================================================
        subroutine update_scene

#include <gl/fgl.h>
#include <gl/fdevice.h>

        common /flags/ mode, NOTHING, IORIENT


        if (mode .eq. IORIENT) call orient

        if (mode .ne. 0) call draw_scene

        return
        end


c==============================================================================
        subroutine orient

#include <gl/fgl.h>
#include <gl/fdevice.h>

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


