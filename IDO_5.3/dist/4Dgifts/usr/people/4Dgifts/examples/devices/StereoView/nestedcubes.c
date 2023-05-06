/*
 *                    Nested cubes example of stereo
 *
 *     If given any arguments, will run in stereo windows mode; a stereo
 *   background program should probably be run before it to avoid a
 *   messy looking screen.
 */

#include <string.h>
#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>

#include "stereo.h"

static Matrix objmat = {
	{1.0, 0.0, 0.0, 0.0},
	{0.0, 1.0, 0.0, 0.0},
	{0.0, 0.0, 1.0, 0.0},
	{0.0, 0.0, 0.0, 1.0},
};

enum { NOTHING, ORIENT, SPIN };
int mode = SPIN;

int omx, mx, omy, my;	/* old and new mouse position */

int use_windows = 0;	/* Hog whole screen by default */

long wid;	/* Window identifier */

/*
 * Local function prototypes
 */
static void initialize(char **);
static void update_scene();
static void orient();
static void spin();
static void draw_scene();
static void draw_cube();

main(int argc, char* argv[])
{
	int dev;
	short val;

	if (argc > 1)
		use_windows = 1;

	initialize(argv);

	draw_scene();

	while (TRUE)
	{
		if (mode==SPIN && !qtest())
		{
			update_scene();
		}
		else switch(dev=qread(&val))
		{
		case ESCKEY:
			if (val) break;	/* exit on up, not down */
		case WINQUIT:
			stereo_off();	/* From libstereo.a */
			if (use_windows)
				st_winclose(wid);
			exit(0);
		case REDRAW:
			if (use_windows) st_redraw(val);
			else
				reshapeviewport();
			draw_scene();
			break;
		case MIDDLEMOUSE:
			omx = mx; omy = my;
			if (val)
			{
				mode = ORIENT;
			}
			else mode = NOTHING;
			break;
		case MOUSEX:
			omx = mx; mx = val;
			/* update_scene(); */
			break;
		case MOUSEY:
			omy = my; my = val;
			update_scene();
			break;
		}	/* End of switch */
	}	/* End of while(TRUE) */
}

static int xmaxscreen, ymaxscreen;

static void
initialize(char **argv)
{
	char *t;	/* Set to name of executable */
	t = strrchr(argv[0], '/');
	if (t == NULL) t = argv[0];
	else t +=1 ;

	xmaxscreen = getgdesc(GD_XPMAX);
	ymaxscreen = getgdesc(GD_YPMAX);

	if (use_windows)
	{
		wid = st_winopen(t);

		st_left(wid);
		doublebuffer();
		RGBmode();
		gconfig();
		mmode(MVIEWING);
		frontbuffer(TRUE); RGBcolor(0, 0, 0); clear(); frontbuffer(FALSE);

		st_right(wid);
		doublebuffer();
		RGBmode();
		gconfig();
		mmode(MVIEWING);
		frontbuffer(TRUE); RGBcolor(0, 0, 0); clear(); frontbuffer(FALSE);
	}
	else
	{
		prefposition(0, xmaxscreen, 0, ymaxscreen);
		wid = winopen(t);

		doublebuffer();
		RGBmode();
		gconfig();
		mmode(MVIEWING);
		viewport(0, xmaxscreen, 0, ymaxscreen);
		frontbuffer(TRUE); RGBcolor(0, 0, 0); clear(); frontbuffer(FALSE);
	}

	qdevice(ESCKEY);
	qdevice(WINQUIT);
	qdevice(MIDDLEMOUSE);
	qdevice(LEFTMOUSE);
	qdevice(MOUSEX);
	qdevice(MOUSEY);

	if (!use_windows)
		stereo_on();	/* From libstereo.a */
}

static void
update_scene()
{
	switch (mode)
	{
	case ORIENT:
		orient();
		break;
	case SPIN:
		spin();
		break;
	}

	if (mode != NOTHING) draw_scene();
}

static void
orient ()
{
	pushmatrix();

	rotate(mx-omx, 'y');
	rotate(omy-my, 'x');

	multmatrix(objmat);
	getmatrix(objmat);

	popmatrix();
}

static void
spin()
{
	static float a, b, c;
	float sa, sb, sc;

	sa = fsin(a);
	sb = fsin(b);
	sc = fsin(c);

	if ((a += 1.0 * M_PI/1800.) > 2.0*M_PI) a -= 2.0*M_PI;
	if ((b += 3.0 * M_PI/1800.) > 2.0*M_PI) b -= 2.0*M_PI;
	if ((c += 7.0 * M_PI/1800.) > 2.0*M_PI) c -= 2.0*M_PI;

	pushmatrix();

	rot(sa, 'y');
	rot(sb, 'x');
	rot(sc, 'z');

	multmatrix(objmat);
	getmatrix(objmat);

	popmatrix();
}

static int fovy = 300;
static float near = 4.0, far = 8.0, conv = 6.0, eye = 0.2;
static float dist = 6.0;

static void
draw_scene() 
{
	int i;
	float aspect =
		(float)xmaxscreen/(float)YMAXSTEREO/2.0;

	/* right eye */

	if (use_windows)
		st_right(wid);
	else
		viewport(0, xmaxscreen, 0, YMAXSTEREO);

	RGBcolor(255, 255, 255); clear();

	pushmatrix();
	stereopersp(fovy, aspect, near, far, conv, eye);
	translate(0.0, 0.0, -dist);
	multmatrix(objmat);
	for (i=0; i<10; i++)
	{
		scale(0.8, 0.8, 0.8);
		rotate(50, 'x');
		rotate(70, 'y');
		rotate(90, 'z');
		draw_cube();
	}
	popmatrix();

	/* left eye */

	if (use_windows)
		st_left(wid);
	else
		viewport(0, xmaxscreen, YOFFSET, YOFFSET+YMAXSTEREO);

	RGBcolor(255, 255, 255); clear();

	pushmatrix();
	stereopersp(fovy, aspect, near, far, conv, -eye);
	translate(0.0, 0.0, -dist);
	multmatrix(objmat);
	for (i=0; i<10; i++)
	{
		scale(0.8, 0.8, 0.8);
		rotate(50, 'x');
		rotate(70, 'y');
		rotate(90, 'z');
		draw_cube();
	}
	popmatrix();

	swapbuffers();

	/*
	 * As of IRIX 3.3, swapbuffers() does not wait for the vertical
	 * retrace period until you try to do another graphics call to
	 * the window (at which time your process will block).  So, by
	 * doing both swapbuffers in a row here, the windows will get
	 * swapped at the same time most of the time.
	 */
	if (use_windows)
	{
		st_right(wid);
		swapbuffers();
	}
}

static float cube_vert[8][3] = {
	{1.0, 1.0, -1.0,},
	{1.0, -1.0, -1.0,},
	{-1.0, -1.0, -1.0,},
	{-1.0, 1.0, -1.0,},
	{1.0, 1.0, 1.0,},
	{1.0, -1.0, 1.0,},
	{-1.0, -1.0, 1.0,},
	{-1.0, 1.0, 1.0,},
};


static void
draw_cube()
{
	RGBcolor(0, 0, 0);

	bgnclosedline();
	v3f(cube_vert[0]);
	v3f(cube_vert[1]);
	v3f(cube_vert[2]);
	v3f(cube_vert[3]);
	endclosedline();

	bgnclosedline();
	v3f(cube_vert[4]);
	v3f(cube_vert[5]);
	v3f(cube_vert[6]);
	v3f(cube_vert[7]);
	endclosedline();

	bgnline();
	v3f(cube_vert[0]);
	v3f(cube_vert[4]);
	endline();

	bgnline();
	v3f(cube_vert[1]);
	v3f(cube_vert[5]);
	endline();

	bgnline();
	v3f(cube_vert[2]);
	v3f(cube_vert[6]);
	endline();

	bgnline();
	v3f(cube_vert[3]);
	v3f(cube_vert[7]);
	endline();
}
