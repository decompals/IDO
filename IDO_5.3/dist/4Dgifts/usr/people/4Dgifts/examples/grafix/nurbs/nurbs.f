c
c nurbs.f
c
c $Revision: 1.4 $
c

#define NUM_KNOTS 	8
#define NUM_COORDS 	3
#define ORDER 		4
#define NUM_POINTS 	(NUM_KNOTS - ORDER)


	program fnurbs
#include <gl/fgl.h>
#include <gl/fdevice.h>
	real*8 surfknots(NUM_KNOTS)
	real*8 trimknots(12)
	real*8 ctlpoints(12,4)
	real*8 trimpoints(9,3)
	real idmat(4,4)
	logical trimflag
	common/nurbs/surfknots,trimknots,ctlpoints,trimpoints,idmat,
     1  trimflag
	common/machzval/izfar

	integer*2 val
	integer dev
	call init_windows
	call setup_queue
	call init_view
	call make_lights
	call set_scene
	call draw_trim_surface

	do while(.true.)
		do while(qtest())
			dev = qread(val)
			if (dev .eq. ESCKEY) then
				call gexit
				stop
			elseif (dev .eq. REDRAW) then
				call reshap
				call set_scene
				call draw_trim_surface
			elseif (dev .eq. LEFTMO) then
				if (val .eq. 0) trimflag = .not. trimflag
			endif
		end do
		call set_scene
		call draw_trim_surface
	end do
	end

	subroutine init_windows
#include <gl/fgl.h>
	common/machzval/izfar

	if (getgde(GDBNDR) .le. 0) then
	    write(0,100)
100	    format('fnurbs: requires double buffered RGB which is unavailable ',
     1		   'on this machine')
	    stop 1
	endif

	gid = winope('fnurbs', 6)
	call wintit('NURBS Surface', 13)
	call double
	call RGBmod
	call gconfi
	call zbuffe(.true.)
	izfar = getgde(GDZMAX)
	call setnur(NERROR, 1.0)
	return
	end

	subroutine setup_queue
#include <gl/fgl.h>
#include <gl/fdevice.h>
	call qdevic(ESCKEY)
	call qdevic(LEFTMO)
	return
	end

	subroutine init_view
#include <gl/fgl.h>

	real*8 surfknots(NUM_KNOTS)
	real*8 trimknots(12)
	real*8 ctlpoints(12,4)
	real*8 trimpoints(9,3)
	real idmat(4,4)
	common/nurbs/surfknots,trimknots,ctlpoints,trimpoints,idmat,
     1  trimflag

	call mmode(MPROJE)
	call ortho(-4.0,4.0,-4.0,4.0,-4.0,4.0)
	call mmode(MVIEWI)
	call loadma(idmat)
	return
	end

	subroutine set_scene
#include <gl/fgl.h>
        common /machzval/izfar

	call lmbind(MATERI,0)
	call RGBcol(150,150,150)
	call lmbind(MATERI,1)
        call czclea($00969696, izfar)
	call rotate(100,'y')
	call rotate(100,'z')
	return
	end

	subroutine draw_trim_surface
#include <gl/fgl.h>
	real*8 surfknots(NUM_KNOTS)
	real*8 trimknots(12)
	real*8 ctlpoints(12,4)
	real*8 trimpoints(9,3)
	real idmat(4,4)
	logical trimflag
	common/nurbs/surfknots,trimknots,ctlpoints,trimpoints,idmat,
     1  trimflag
	call bgnsur
	call nurbss(NUM_KNOTS,surfknots,NUM_KNOTS,surfknots,
     1		8*NUM_COORDS, 8*NUM_COORDS*NUM_POINTS,
     1		ctlpoints,ORDER,ORDER,NV3D)
	if( trimflag ) then
            call bgntri
            call nurbsc(12, trimknots, 8*NUM_COORDS, trimpoints, 3, NP2DR)
            call endtri
	endif
	call endsur
	call swapbu
	return
	end

	subroutine make_lights
#include <gl/fgl.h>
	real array(19)
	data array/	EMISSI,0.0,0.0,0.0,
     1			AMBIEN,0.1,0.1,0.1,
     2			DIFFUS,0.6,0.3,0.3,
     3			SPECUL,0.0,0.6,0.0,
     4			SHININ,2.0,
     5			LMNULL              /

        call lmdef(DEFLMO,1,0,0)
	call lmdef(DEFLIG,1,0,0)
	call lmdef(DEFMAT,1,19,array)
	call lmbind(LIGHT0,1)
	call lmbind(LMODEL,1)
	return
	end

	block data 

	real*8 surfknots(NUM_KNOTS)
	real*8 trimknots(12)
	real*8 ctlpoints(12,4)
	real*8 trimpoints(9,3)
	real idmat(4,4)
	logical trimflag
	common/nurbs/surfknots,trimknots,ctlpoints,trimpoints,idmat,
     1  trimflag

	data trimflag /.false./

	data idmat/	1.0,0.0,0.0,0.0,
     1			0.0,1.0,0.0,0.0,
     2			0.0,0.0,1.0,0.0,
     3			0.0,0.0,0.0,1.0 /

	data surfknots/-1.d0,-1.d0,-1.d0,-1.d0,1.d0,1.d0,1.d0,1.d0/

	data trimknots/0.d0,0.d0,0.d0,1.d0,1.d0,2.d0,2.d0,3.d0,3.d0,
     1  	4.d0,4.d0,4.d0/

	data ctlpoints/
     1		-2.5d0,  -3.7d0,  1.d0,
     1		-1.5d0,  -3.7d0,  3.d0,
     1		1.5d0,  -3.7d0, -2.5d0,
     1		2.5d0,  -3.7d0,  -.75d0,

     1		-2.5d0,  -2.d0,  3.d0,
     1		-1.5d0,  -2.d0,  4.d0,
     1		1.5d0,  -2.d0,  -3.d0,
     1		2.5d0,  -2.d0,  0.d0,

     1		-2.5d0, 2.d0,  1.d0,
     1		-1.5d0, 2.d0,  0.d0,
     1		1.5d0,  2.d0,  -1.d0,
     1		2.5d0,  2.d0,  2.d0,

     1		-2.5d0,  2.7d0,  1.25d0,
     1		-1.5d0,  2.7d0,  .1d0,
     1		1.5d0,  2.7d0,  -.6d0,
     1		2.5d0,  2.7d0,  .2d0 /

	data trimpoints/
     1		1.d0, 0.d0, 1.d0,
     1		1.d0, 1.d0, 1.d0,
     1		0.d0, 2.d0, 2.d0,
     1		-1.d0, 1.d0, 1.d0,
     1		-1.d0, 0.d0, 1.d0,
     1		-1.d0, -1.d0, 1.d0,
     1		0.d0, -2.d0, 2.d0, 
     1		1.d0, -1.d0, 1.d0,
     1		1.d0, 0.d0, 1.d0 /

	end
