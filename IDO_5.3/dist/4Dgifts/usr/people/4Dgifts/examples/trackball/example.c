/*
 *    example.c:
 *
 * This simple example program demonstrates the use of the trackball
 * user interface and the extensible event handling routines.
 *
 * Written by Gavin Bell for Silicon Graphics, Inc.
 */
#include <gl/gl.h>
#include <gl/device.h>
#include "ui.h"

/*
 * This quaternion will keep track of the object's total rotation.
 * 
 * Note: See the following for more information on quaternions:
 * 
 * - Shoemake, K., Animating rotation with quaternion curves, Computer
 *   Graphics 19, No 3 (Proc. SIGGRAPH'85), 245-254, 1985.
 * - Pletinckx, D., Quaternion calculus as a basic tool in computer
 *   graphics, The Visual Computer 5, 2-13, 1989.
 */
static float rvec[4] = {0.0, 0.0, 0.0, 1.0};

/*
 * And this will keep track of the total xyz translation
 */
static float trans[3] = { 0.0, 0.0, 0.0};

/*  ---  Global modes ---
 *
 * Should the object keep spinning when let go?
 */
static int spin = TRUE;
/*
 * Is the user interface active?
 */
static int uiflag = TRUE;

/*	--- Local function prototypes ---
 *
 */
void remember_view(float *, float *), spin_draw(), do_menus();
void draw_whatever();

int
main()
{
	/* Pre-declare some functions defined later */

	init_windows();
	init_menus();

/*
 * Add an event to look for:  if the RIGHTMOUSE goes DOWN in ANY
 * window, call the do_menus function with argument 0.
 */
	add_event(ANY, RIGHTMOUSE, DOWN, do_menus, 0);
	qdevice(RIGHTMOUSE);
/*
 * Now define a couple of events to quit the program:
 * 
 * If the escape key goes UP in any window, call the ui_exit function
 * with argument 0.
 */
	add_event(ANY, ESCKEY, UP, ui_exit, 0);
	qdevice(ESCKEY);
/*
 * If we get a WINQUIT event for any window, also call ui_exit
 */
	add_event(ANY, WINQUIT, ANY, ui_exit, 0);
	qdevice(WINQUIT);
/*
 * And we had better redraw the window if we get a REDRAW event.
 */
	add_event(ANY, REDRAW, ANY, draw_whatever, 0);
	qdevice(REDRAW);
/*
 * This adds a function to be called when spin mode is on.
 * spin_draw(0) will be called when the spin variable is non-zero.
 */
	add_update(&spin, spin_draw, 0);
/*
 * Now start interaction with the user.
 */
	ui(remember_view);
/*
 * ui() won't return until ui_exit() is called in response to the
 * ESCKEY or a WINQUIT signal (see the add_event code above).  When it
 * does return, we'll just end the program.
 */
	gexit();

	return 0;
}

/*
 * We remember the last rotation (expressed as a quaternion) so that
 * we can keep on applying it if we want the object to spin when the
 * user lets go of it.
 */
static float spinrot[4] = {0.0, 0.0, 0.0, 1.0};

/*
 * This function is called by the ui() routine with two arguments; the
 * quaternion representing the current incremental rotation, and
 * an array of three floats that is the incremental translation.
 */
void
remember_view(float *r, float *t)
{
	Matrix m;
	register int i;

	vcopy(r, spinrot);	/* Remember the rotation so we can spin... */
	spinrot[3] = r[3];	/* (vcopy only copies 3 of the 4...) */
	add_quats(r, rvec, rvec);	/* And add on more rotation */

	vadd(t, trans, trans);	/* And more translation */

	draw_whatever();	/* And draw the scene */
}

/*
 * This function is used when the trackball isn't active; it
 * continuously redraws the scene, rotating by the same amount as the
 * last interaction the user had with the trackball.
 */
void
spin_draw()
{
	/*
	 *	The ui_quiet flag is defined in "ui.h", and is
	 * kept up-to-date by the ui() routines.  When
	 * ui_quiet is FALSE, leave the object alone to let the
	 * user interact with it.
	 */
	if (ui_quiet)
	{
		add_quats(spinrot, rvec, rvec); /* Apply rotation again */
		draw_whatever();	/* And draw the scene */
	}
}

/*
 * The vertices of the cube
 */
float cv0[3] = {-1.0, -1.0, -1.0 };
float cv1[3] = {-1.0, -1.0, 1.0 };
float cv2[3] = {-1.0, 1.0, 1.0 };
float cv3[3] = {-1.0, 1.0, -1.0 };
float cv4[3] = {1.0, -1.0, -1.0 };
float cv5[3] = {1.0, -1.0, 1.0 };
float cv6[3] = {1.0, 1.0, 1.0 };
float cv7[3] = {1.0, 1.0, -1.0 };

/*
 * This can be replaced with whatever drawing function you desire; for
 * simplicity, this one just draws a cube.
 */
void
draw_whatever()
{
	Matrix m;

	pushmatrix();

/*
 *	First, translate
 */
	translate(trans[0], trans[1], trans[2]);

/*
 *	Then scale/rotate the object
 */
	scale(0.2, 0.2, 0.2);	/* Make the cube smaller */

	build_rotmatrix(m, rvec);
	multmatrix(m);

	color(0);	/* Erase window to black */
	clear();

	color(7) ;
	bgnpolygon();
	v3f(cv0) ; v3f(cv1) ; v3f(cv2) ; v3f(cv3) ;
	endpolygon();

	color(1) ;
	bgnpolygon() ;
	v3f(cv3) ; v3f(cv2) ; v3f(cv6) ; v3f(cv7) ;
	endpolygon() ;

	color(2) ;
	bgnpolygon() ;
	v3f(cv7) ; v3f(cv6) ; v3f(cv5) ; v3f(cv4) ;
	endpolygon() ;

	color(3) ;
	bgnpolygon() ;
	v3f(cv4) ; v3f(cv5) ; v3f(cv1) ; v3f(cv0) ;
	endpolygon() ;

	color(4) ;
	bgnpolygon() ;
	v3f(cv1) ; v3f(cv5) ; v3f(cv6) ; v3f(cv2) ;
	endpolygon() ;

	color(5) ;
	bgnpolygon() ;
	v3f(cv0) ; v3f(cv3) ; v3f(cv7) ; v3f(cv4) ;
	endpolygon() ;

	swapbuffers();	/* Show what we just drew */
	popmatrix();
}

/*
 *	init_windows just opens a window and sets up drawing modes,
 * etc.
 */
init_windows()
{
	winopen("Example");
	cmode();
	doublebuffer();
	gconfig();
	backface(TRUE);

	/* Set up 3D to 2D transformation */
	perspective(400, 1.0, 0.1, 4.0);

	/* And move the eye away from the center of the world */
	translate(0.0, 0.0, -2.0);
}

static long mymenu;

/*
 *	init_menus sets up menus that do useful things
 */
init_menus()
{
	/* Functions used by the menu */
	void reset_transforms(), toggle_spin();
	void toggle_ui(), do_menus();

	mymenu = newpup();
	addtopup(mymenu, "Example %t");
	addtopup(mymenu, "Reset Transformations %f", reset_transforms);
	addtopup(mymenu, "Spin On/Off %f", toggle_spin);
	addtopup(mymenu, "Interface On/Off %f", toggle_ui);
	addtopup(mymenu, "Exit Program %f", ui_exit);
}

/*
 * The functions used by the menus.  Called automagically by dopup
 * and add_event.  We could have just done:
 * 
 * add_event(ANY,RIGHTMOUSE,DOWN,dopup,mymenu);
 *
 * In a real program, you usually re-build your menu structure so the
 * entries always reflect the state of the programs internal flags; in
 * that case you can't just call dopup().
 */
void
do_menus()
{
	dopup(mymenu);
}

void
reset_transforms()
{
    rvec[0] = rvec[1] = rvec[2] = 0.0;
    rvec[3] = 1.0;
    trans[0] = trans[1] = trans[2] = 0.0;
}

void
toggle_spin()
{
	spin = !spin;
}

void
toggle_ui()
{
	uiflag = !uiflag;
	ui_active(uiflag);
}
