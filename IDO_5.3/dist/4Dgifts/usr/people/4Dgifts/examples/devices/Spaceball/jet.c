/*
 * jet.c
 */
#include <stdio.h>
#include <math.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <gl/spaceball.h>

#define vp_llx 25
#define vp_lly 25
#define vp_urx 1000
#define vp_ury 750

#define MAIN_OBJECT 1
#define SUB_OBJECT 2
#define EDIT_MAIN 3

#define min_int -2000000000

#define	NORMAL	0
#define TRATE	1
#define RRATE	2

#define veclength( v )	sqrt( v[0]*v[0] + v[1]*v[1] + v[2]*v[2] )

#define	IDENTITY_MATRIX		{ \
	{ 1, 0, 0, 0 }, \
	{ 0, 1, 0, 0 }, \
	{ 0, 0, 1, 0 }, \
	{ 0, 0, 0, 1 }  \
  }

int
  throwaway = TRUE,		/* flag to cause first piece of data from
				 * Spaceball following input focus to be
				 * ignored as it may be a large change */
  prompt_request = TRUE,
  redisplay = 1;		    /* Causes the image to be redisplayed */

int
  ballmode = NORMAL,
  translation_mode	= 2,
  rotation_mode 	= 2;
float
  translation_rate = .001,
  rotation_rate = .003;
char
  *mode_text[] = { "off", "xyz only", "on" };

float
  objradius = 950.0;	/* guesstimate of radius of object's enclosing sphere */

Matrix
  vmatrix = IDENTITY_MATRIX,
  identitymat = IDENTITY_MATRIX;

float   stw;

void
    enter_double(void),
    draw_main(void),
    exit_double(void),
    vimage(void);

void cuemap (Colorindex, Colorindex,
	    RGBvalue, RGBvalue, RGBvalue, RGBvalue, RGBvalue, RGBvalue);

void process_sb(Device, short), process_sbbut(Device, short);
void spaceball_translate(unsigned short, float []);
void spaceball_rotate(unsigned short period, float []);
void trate_rotate(unsigned short, float []);
void rrate_rotate(unsigned short period, float []);
void mult3x3parts(Matrix, Matrix, Matrix);
void dominant(float [3]), show_matrix(Matrix);
void make_image(void);
void keypad_usage(void);
void rate_pressed(void);
void home(void);
void init_spaceball(void);
void	spaceball_button1(void),
	spaceball_button2(void),
	spaceball_button3(void),
	spaceball_button4(void),
	spaceball_button5(void),
	spaceball_button6(void),
	spaceball_button7(void),
	spaceball_button8(void),
	spaceball_pick_button(void);

main (int argc, char *argv[])
{
    int win;

    keepaspect (getgdesc(GD_XPMAX), getgdesc(GD_YPMAX));
    win = winopen ("Spaceball Demonstration");
    if (win < 0) {
	printf ( "Could not open graphics window\n" );
	exit(1);
    }
    enter_double ();
    cuemap (288, 32, 255, 0, 0, 20, 0, 0);

    qdevice (PADENTER);
    qdevice (PADPF1);
    qdevice (KEYBD);
    qdevice (REDRAW);
    qdevice (INPUTCHANGE);
    qdevice (ESCKEY);
    qdevice (RIGHTMOUSE);

    init_spaceball ();
    home ();

    makeobj (SUB_OBJECT);
    make_image ();
    closeobj ();

    stw = (2.0 * min_int) / (vp_urx - vp_llx);
				/* converts physical to user	 */

    makeobj (MAIN_OBJECT);
    pushmatrix ();
    maketag (EDIT_MAIN);
    multmatrix (vmatrix);
    callobj (SUB_OBJECT);

    popmatrix ();
    closeobj ();

    vimage ();

    exit(0);
}

void draw_main (void)
{
    color (BLACK);
    clear ();
    gflush ();
    callobj (MAIN_OBJECT);
}

void enter_double(void)
{
    doublebuffer ();
    gconfig ();
    frontbuffer (TRUE);
    color (BLACK);
    clear ();
    frontbuffer (FALSE);
}

void exit_double (void)
{
    color (BLACK);
    clear ();
    swapbuffers ();
}

void vimage (void)
{
    Device dev;
    short val;
    Icoord x=0, y=0;
    short but, oldx, oldy;
    register dx, dy;
    Boolean motion = FALSE;
    float   nose_speed;
    float   nose_factor = 0.0;
    short   active = TRUE;

    perspective (500, (vp_urx - vp_llx) / (float) (vp_ury - vp_lly), 
		    1.0e-4, 1.0e4);
    setdepth (0, 0x7fff);
    reshapeviewport ();
    shaderange (288, 288 + 32, 0, 0x7fff);
    depthcue (FALSE);
    pushmatrix ();
    frontbuffer (TRUE);
    draw_main ();
    frontbuffer (FALSE);
    popmatrix ();
    gflush ();

    while (1) {
	oldx = x;
	oldy = y;
	x = getvaluator (MOUSEX);
	y = getvaluator (MOUSEY);
	dx = x - oldx;
	dy = y - oldy;
	but = (getbutton (LEFTMOUSE) << 2) +
	    (getbutton (MIDDLEMOUSE) << 1) +
	    getbutton (RIGHTMOUSE);
	do {
	    dev = qread (&val);

	    if (ISSBALL( dev ))
		process_sb( dev, val );
	    else if (ISSBALLBUT( dev ))
		process_sbbut( dev, val );
	    else if (dev == ESCKEY) {
		if (val == 0)		/* go on key up */
		    goto quit;
	    } else if (dev == KEYBD) {
		if (val == 'm')
		    motion = !motion;
	    } else if ((dev == PADENTER) && (val == 0)) {
		depthcue (!getdcm ());
		if (getdcm ())
		    shaderange (288, 288 + 32, 0, 0x7fff);
	    } else if (dev == INPUTCHANGE) {
		active = val;
		if (!active) {
		    frontbuffer (TRUE);
		    draw_main ();
		    frontbuffer (FALSE);
		} else {
		    sbprompt();
		    throwaway = TRUE;	/* flag to ignore first possibly
					 * large change from Spaceball */
		}
	    } else if (dev == REDRAW) {
		reshapeviewport ();
		frontbuffer (TRUE);
		draw_main ();
		frontbuffer (FALSE);
	    }
	    if (!active && !motion) {
		while (!qtest ())
		    swapbuffers ();
	    }
	} while (qtest ());

	pushmatrix ();

	if (active) {
	    if (but == 4) {	/* rotate x	 */
		loadmatrix (vmatrix);
		rotate (dx * 4, 'x');
	    }
	    else if (but == 2) {/* rotate y	 */
		loadmatrix (vmatrix);
		rotate (dx * 4, 'y');
	    }
	    else if (but == 1) {/* rotate z	 */
		loadmatrix (vmatrix);
		rotate (dx * 4, 'z');
	    }
	    else if (but == 6) {
		loadmatrix (identitymat);
		translate (dx * stw, dy * stw, 0.0);
		multmatrix (vmatrix);
	    }
	    else if (but == 5) {
		loadmatrix (identitymat);
		scale (.98,.98,.98);
		multmatrix (vmatrix);
	    }
	    else if (but == 3) {
		loadmatrix (identitymat);
		scale (1.02, 1.02, 1.02);
		multmatrix (vmatrix);
	    }
	    else if (but == 7)
		break;
	}			/*  if active	 */
	if ((but > 0) && active) {
	    getmatrix (vmatrix);
	    editobj (MAIN_OBJECT);
	    objreplace (EDIT_MAIN);
	    multmatrix (vmatrix);
	    closeobj ();
	}
	else if (motion) {
	    loadmatrix (vmatrix);
	    rotate (30, 'z');
	    getmatrix(vmatrix);

	    nose_speed = cos (nose_factor);
	    nose_speed = nose_speed * 5.0;
	    rotate ((Angle) nose_speed, 'x');
	    nose_factor = nose_factor + 0.05;
	    if (nose_factor > 31415.0)
		nose_factor = 0.0;

	    getmatrix (vmatrix);
	    editobj (MAIN_OBJECT);
	    objreplace (EDIT_MAIN);
	    multmatrix (vmatrix);
	    closeobj ();
	}
	popmatrix ();
	if (prompt_request) {
	    sbprompt();
	    prompt_request = FALSE;
	}
	if (redisplay) {
	    editobj (MAIN_OBJECT);
	    objreplace (EDIT_MAIN);
	    multmatrix (vmatrix);
	    closeobj ();
            draw_main ();
            swapbuffers ();
	    redisplay = 0;
	}
    }

quit:
    /* just exit	 */
    exit_double ();
}

void cuemap (Colorindex firstcolor, Colorindex cuenum,
    RGBvalue lastred, RGBvalue lastgreen, RGBvalue lastblue,
    RGBvalue firstred, RGBvalue firstgreen, RGBvalue firstblue)
{
    Colorindex i;
    float   incred,
	    incgreen,
	    incblue;
    float   nextred,
	    nextgreen,
	    nextblue;

    incred = (lastred - firstred) / (float) cuenum;
    incgreen = (lastgreen - firstgreen) / (float) cuenum;
    incblue = (lastblue-firstblue) / (float) cuenum;
    nextred = firstred;
    nextgreen = firstgreen;
    nextblue = firstblue;
    for (i = 0; i < cuenum; i++) {
	mapcolor (firstcolor + i, (RGBvalue) nextred,
		(RGBvalue) nextgreen, (RGBvalue) nextblue);
	nextred += incred;
	nextgreen += incgreen;
	nextblue += incblue;
    }

}

void process_sb( Device dev, short val )
{
    static unsigned short myperiod;
    static float  mytvec[3], myrvec[3];

    switch (dev) {
      case SBPERIOD:
	myperiod = val;
	break;
      case SBTX:
	mytvec[0] = ldexp( (double) val, -14 );
	break;
      case SBTY:
	mytvec[1] = ldexp( (double) val, -14 );
	break;
      case SBTZ:
	mytvec[2] = ldexp( (double) val, -14 );
	spaceball_translate( myperiod, mytvec );
	break;
      case SBRX:
	myrvec[0] = ldexp( (double) val, -14 );
	break;
      case SBRY:
	myrvec[1] = ldexp( (double) val, -14 );
	break;
      case SBRZ:
	myrvec[2] = ldexp( (double) val, -14 );
	spaceball_rotate( myperiod, myrvec );
	break;
      default:
	printf( "Call to process_sbbut with non-spaceball event\n" );
    }
}

void process_sbbut(Device dev, short val)
{
    if (!val)
	return;

    if (ballmode != NORMAL) {
	rate_pressed();
	return;
    }

    switch (dev) {
    case SBBUT1:
	spaceball_button1();
	break;
      case SBBUT2:
	spaceball_button2();
	break;
      case SBBUT3:
	spaceball_button3();
	break;
      case SBBUT4:
	spaceball_button4();
	break;
      case SBBUT5:
	spaceball_button5();
	break;
      case SBBUT6:
	spaceball_button6();
	break;
      case SBBUT7:
	spaceball_button7();
	break;
      case SBBUT8:
	spaceball_button8();
	break;
      case SBPICK:
	spaceball_pick_button();
	break;
    }
}

void spaceball_translate(unsigned short period, float vec[])
{
    float scale_fac;
    float objdistance, unitfactor, unitvec[3];

    prompt_request = TRUE;
    if (throwaway)		/* ignore first piece of data? */
	return;		/* will be changed in spaceball_rotate */

    if (translation_mode == 1)
	dominant( vec );
    if (translation_mode != 0) {
        scale_fac = period * translation_rate;
        if (vmatrix[3][2] > -objradius) {
	    scale_fac *= objradius;
            vmatrix[3][0] += scale_fac * vec[0];
            vmatrix[3][1] += scale_fac * vec[1];
            vmatrix[3][2] -= scale_fac * vec[2];
        } else {	/* Z movement causes movement to/from eyepoint */
	    scale_fac *= objdistance = veclength( vmatrix[3] );
	    unitfactor = 1.0 / objdistance;
	    unitvec[0] = vmatrix[3][0] * unitfactor;
	    unitvec[1] = vmatrix[3][1] * unitfactor;
	    unitvec[2] = vmatrix[3][2] * unitfactor;
    
	    /* the optimizer had problems with the following code and did not
	     * update vmatrix[3][2] !! */
    
            vmatrix[3][0] += scale_fac * (vec[0] + unitvec[0] * vec[2]);
            vmatrix[3][1] += scale_fac * (vec[1] + unitvec[1] * vec[2]);
            vmatrix[3][2] += scale_fac *           unitvec[2] * vec[2];
        }
        redisplay = TRUE;
    }
}


void spaceball_rotate(unsigned short period, float vec[])
{
    Matrix delta_matrix;

    prompt_request = TRUE;
    if (throwaway) {		/* ignore first piece of data? */
	throwaway = FALSE;
	return;
    }

    switch (ballmode) {
      case NORMAL:
	break;
      case RRATE:
	rrate_rotate( period, vec );
	return;
      case TRATE:
	trate_rotate( period, vec );
	return;
    }

    vec[0] = -vec[0];
    vec[1] = -vec[1];
    if (rotation_mode == 1)
	dominant( vec );
    if (rotation_mode != 0) {
	rotarbaxis( period * rotation_rate, vec[0], vec[1], vec[2],
	    delta_matrix
	);
	mult3x3parts( vmatrix, delta_matrix, vmatrix );
	redisplay = TRUE;
    }
}

void home(void)
{
    memcpy( vmatrix, identitymat, sizeof( Matrix ));
    vmatrix[3][2] = -3330.0;
}

void init_spaceball(void)
{
    if (!sbexists())
	fprintf( stderr, "Cannot initialize the spaceball\n" );

    qdevice( SBTX );
    qdevice( SBTY );
    qdevice( SBTZ );
    qdevice( SBRX );
    qdevice( SBRY );
    qdevice( SBRZ );
    qdevice( SBPERIOD );
    qdevice( SBPICK );
    qdevice( SBBUT1 );
    qdevice( SBBUT2 );
    qdevice( SBBUT3 );
    qdevice( SBBUT4 );
    qdevice( SBBUT5 );
    qdevice( SBBUT6 );
    qdevice( SBBUT7 );
    qdevice( SBBUT8 );
    sbprompt();

    return;
}

void spaceball_button1(void)
{
    sbbeep( "cC" );             /* short beep, short pause */
    switch (++translation_mode) {
      case 1:
        sbbeep( "cE" );         /* one more short beeps to indicate on */
        break;
      case 2:
        sbbeep( "cCcE" );       /* two more short beeps to indicate
    			     * xyz only mode */
        break;
      case 3:
        translation_mode = 0;
        break;
    }
    printf( "Translations %s\n", mode_text[ translation_mode ] );
}

void spaceball_button2(void)
{
    sbbeep( "cC" );             /* short beep, short pause */
    switch (++rotation_mode) {
      case 1:
        sbbeep( "cE" );         /* one more short beeps to indicate on */
        break;
      case 2:
        sbbeep( "cCcE" );       /* two more short beeps to indicate
    			     * xyz only mode */
        break;
      case 3:
        rotation_mode = 0;
        break;
    }
    printf( "Rotations %s\n", mode_text[ rotation_mode ] );
}

void spaceball_button3(void)
{
    sbbeep( "cC" );             /* short beep, short pause */
    printf( "matrix:\n" );
    show_matrix( vmatrix );
}

void spaceball_button4(void)
{
    sbbeep( "eC" );             /* normal beep, short pause */
}

void spaceball_button5(void)
{
    sbbeep( "cC" );             /* short beep, short pause */
    ballmode = TRATE;
    printf( "\rTranslation rate = %12.6f", translation_rate );
    fflush( stdout );
}

void spaceball_button6(void)
{
    sbbeep( "cC" );             /* short beep, short pause */
    ballmode = RRATE;
    printf( "\rRotation rate = %12.6f", rotation_rate );
    fflush( stdout );
}

void spaceball_button7(void)
{
    sbbeep( "cC" );             /* short beep, short pause */
    keypad_usage();
}

void spaceball_button8(void)
{
    sbbeep( "cE" );             /* short beep, normal pause */
    printf( "Rezeroing Spaceball\n" );
    /* perform rezeroing function which can be used to generate or
     * stop drift */
    sbrezero();
}

void spaceball_pick_button(void)
{
    sbbeep( "cE" );             /* short beep, normal pause */
    /* reset vmatrix */
    home();
    redisplay = TRUE;
}

void keypad_usage(void)
{
    int i;
    static char *keypad_usage_text[] = {
" +-----------------+-----------------+-----------------+-----------------+",
" |     - 1 -       |     - 2 -       |     - 3 -       |     - 4 -       |",
" |     Toggle      |     Toggle      |   Print the     |                 |",
" |  Translations   |   Rotations     |    matrix       |                 |",
" | off/xyz_only/on | off/xyz_only/on |                 |                 |",
" |                 |                 |                 |                 |",
" |-----------------+-----------------+-----------------+-----------------|",
" |     - 5 -       |     - 6 -       |     - 7 -       |     - 8 -       |",
" |      Set        |      Set        |   Print this    | Rezero the ball |",
" |  Translation    |   Rotation      |    message      |                 |",
" |     rate        |     rate        |                 |                 |",
" |                 |                 |                 |                 |",
" +-----------------+-----------------+-----------------+-----------------+",
"                          PICK BUTTON: Reset object                       "
    };

    for (i=0; i<(sizeof(keypad_usage_text)/sizeof(keypad_usage_text[0])); i++)
    printf( "%s\n", keypad_usage_text[i] );
}

void rate_pressed(void)
{
    sbbeep( "bBbE" );
    ballmode = NORMAL;
    putchar( '\n' );
}


void trate_rotate(unsigned short period, float vec[])
{
    translation_rate *= exp( period * vec[1] * -0.001 );
    printf( "\rTranslation rate = %12.6f", translation_rate );
    fflush( stdout );
}

void rrate_rotate(unsigned short period, float vec[])
{
    rotation_rate *= exp( period * vec[1] * -0.001 );
    printf( "\rRotation rate = %12.6f", rotation_rate );
    fflush( stdout );
}

void dominant(float vec[3])
{
    if (fabs( vec[0] ) > fabs( vec[1] )) {
	vec[1] = 0.0;
	if (fabs( vec[0] ) > fabs( vec[2] ))
	    vec[2] = 0.0;
	else
	    vec[0] = 0.0;
    } else {
	vec[0] = 0.0;
	if (fabs( vec[1] ) > fabs( vec[2] ))
	    vec[2] = 0.0;
	else
	    vec[1] = 0.0;
    }
}

void show_matrix(Matrix m)
{
    int i,j;

    for (i=0; i<4; i++) {
	for (j=0; j<4; j++)
	    printf( "  %12.6f", m[i][j] );
	putchar( '\n' );
    }
}

void mult3x3parts(Matrix mat1, Matrix mat2, Matrix result)
{
    Matrix tmp;
    int i, j;

    for (i=0; i<3; i++)
	for (j=0; j<3; j++)
	    tmp[i][j] = mat1[i][0] * mat2[0][j]
		      + mat1[i][1] * mat2[1][j]
		      + mat1[i][2] * mat2[2][j];
    for (i=0; i<3; i++)
	for (j=0; j<3; j++)
	    result[i][j] = tmp[i][j];
}


void make_image (void)
{
    color(RED);
    movei(-105,-163,-128);
    drawi(-88,-350,-135);
    drawi(-76,-611,-101);
    drawi(-60,-759,-115);
    drawi(-59,-759,-97);
    drawi(-54,-759,-82);
    drawi(0,-1000,-160);
    drawi(0,-759,-180);
    drawi(0,-611,-179);
    drawi(0,-350,-174);
    drawi(0,-163,-161);
    drawi(105,-163,-128);
    drawi(187,-163,-75);
    drawi(196,-165,-84);
    drawi(197,-178,-84);
    drawi(198,-178,-183);
    drawi(198,-176,-183);
    drawi(189,3,-181);
    drawi(189,3,-98);
    drawi(196,-165,-84);
    drawi(198,-176,-183);
    movei(-105,-163,-128);
    drawi(0,-163,-161);
    movei(-105,-163,-129);
    drawi(-105,3,-129);
    drawi(-85,160,-121);
    drawi(0,160,-134);
    drawi(0,337,-118);
    drawi(0,401,-111);
    drawi(0,536,-101);
    drawi(0,667,-84);
    drawi(0,828,-64);
    drawi(0,972,-48);
    drawi(106,972,-48);
    drawi(144,972,-49);
    drawi(147,827,-66);
    drawi(158,833,-106);
    drawi(162,704,-141);
    drawi(144,667,-84);
    drawi(147,827,-66);
    drawi(165,828,-57);
    drawi(175,828,-18);
    drawi(149,972,8);
    drawi(159,972,-43);
    drawi(165,828,-57);
    drawi(181,667,-75);
    drawi(144,667,-84);
    drawi(140,827,-66);
    drawi(144,972,-49);
    drawi(155,950,-74);
    drawi(158,833,-106);
    drawi(140,827,-66);
    drawi(98,828,-65);
    drawi(106,972,-48);
    movei(-105,-163,-129);
    drawi(0,-163,-162);
    drawi(0,3,-149);
    drawi(0,160,-134);
    drawi(85,160,-121);
    drawi(105,3,-129);
    drawi(182,3,-94);
    drawi(187,-163,-75);
    drawi(199,-163,-75);
    drawi(302,-163,-82);
    drawi(311,3,-70);
    drawi(316,157,-60);
    drawi(316,326,-45);
    drawi(316,432,-52);
    drawi(316,537,-64);
    drawi(316,667,-85);
    drawi(362,682,-89);
    drawi(618,768,-110);
    drawi(618,713,-105);
    drawi(362,563,-70);
    drawi(362,682,-89);
    drawi(362,562,-85);
    drawi(618,713,-111);
    drawi(618,713,-105);
    drawi(618,669,-104);
    drawi(362,468,-60);
    drawi(362,563,-70);
    drawi(316,537,-64);
    movei(-105,3,-129);
    drawi(0,3,-149);
    drawi(105,3,-129);
    drawi(105,-163,-129);
    drawi(187,-163,-75);
    drawi(106,-163,-29);
    drawi(0,-163,25);
    drawi(0,3,7);
    drawi(0,160,-8);
    drawi(0,337,-17);
    drawi(0,450,-19);
    drawi(0,536,-19);
    drawi(0,667,-12);
    drawi(0,828,-5);
    drawi(0,972,-4);
    drawi(149,972,8);
    drawi(110,828,-12);
    drawi(111,667,-16);
    drawi(139,722,-4);
    drawi(142,557,-4);
    drawi(149,515,-6);
    drawi(158,557,-9);
    drawi(162,722,-10);
    drawi(172,667,-54);
    drawi(175,828,-18);
    drawi(157,829,-9);
    drawi(162,722,-10);
    drawi(232,904,214);
    drawi(223,953,216);
    drawi(157,829,-9);
    drawi(149,929,-6);
    drawi(149,972,8);
    drawi(148,929,-6);
    drawi(214,1000,219);
    drawi(209,954,220);
    drawi(143,831,-5);
    drawi(148,929,-6);
    movei(-106,-163,-29);
    drawi(-85,-350,-13);
    drawi(-71,-611,-67);
    drawi(-52,-611,-33);
    drawi(0,-350,42);
    drawi(0,-163,25);
    drawi(-106,-163,-29);
    drawi(-95,3,-35);
    drawi(-90,160,-43);
    drawi(-86,337,-40);
    drawi(0,337,-17);
    drawi(86,337,-40);
    drawi(142,450,-49);
    drawi(168,536,-50);
    drawi(173,667,-54);
    drawi(183,667,-74);
    drawi(196,667,-85);
    drawi(195,536,-68);
    drawi(180,536,-56);
    drawi(183,667,-74);
    movei(-106,972,-48);
    drawi(-98,828,-65);
    drawi(-89,667,-85);
    drawi(0,667,-84);
    drawi(89,667,-84);
    drawi(196,667,-86);
    drawi(196,536,-84);
    drawi(184,401,-91);
    drawi(176,337,-96);
    drawi(181,160,-105);
    drawi(182,3,-94);
    drawi(189,3,-98);
    drawi(185,160,-109);
    drawi(181,160,-105);
    drawi(85,160,-121);
    drawi(88,337,-107);
    drawi(176,337,-96);
    drawi(182,337,-100);
    drawi(183,404,-101);
    drawi(184,401,-91);
    drawi(88,401,-102);
    drawi(0,401,-111);
    drawi(-88,401,-102);
    drawi(-87,536,-93);
    drawi(0,536,-101);
    drawi(87,536,-93);
    drawi(196,536,-84);
    movei(-106,972,-48);
    drawi(0,972,-48);
    movei(-110,828,-12);
    drawi(0,828,-5);
    drawi(110,828,-12);
    drawi(143,831,-5);
    drawi(139,722,-4);
    drawi(209,904,220);
    drawi(209,954,220);
    movei(-111,667,-16);
    drawi(-110,828,-12);
    drawi(-143,831,-5);
    drawi(-139,722,-4);
    drawi(-111,667,-16);
    drawi(0,667,-12);
    drawi(111,667,-16);
    movei(-112,667,-16);
    drawi(0,667,-12);
    drawi(112,667,-16);
    drawi(119,536,-40);
    drawi(142,450,-49);
    drawi(179,450,-57);
    drawi(180,536,-56);
    drawi(168,536,-50);
    movei(-119,536,-40);
    drawi(-112,667,-16);
    movei(-119,536,-40);
    drawi(0,536,-19);
    drawi(119,536,-40);
    movei(-128,-350,-83);
    drawi(-76,-611,-83);
    drawi(-59,-759,-97);
    drawi(0,-1000,-160);
    drawi(0,-759,-54);
    drawi(0,-611,51);
    drawi(0,-350,42);
    drawi(52,-611,-33);
    drawi(71,-611,-67);
    drawi(76,-611,-83);
    drawi(128,-350,-83);
    drawi(187,-163,-75);
    drawi(180,3,-59);
    drawi(192,3,-59);
    drawi(199,-163,-75);
    drawi(197,-178,-84);
    drawi(211,-322,-97);
    drawi(291,-322,-97);
    drawi(301,-178,-184);
    drawi(301,-176,-184);
    drawi(302,-163,-82);
    drawi(312,3,-78);
    drawi(317,157,-78);
    drawi(341,157,-67);
    drawi(362,205,-66);
    drawi(362,235,-58);
    drawi(362,371,-54);
    drawi(362,468,-60);
    drawi(316,432,-52);
    movei(-128,-350,-83);
    drawi(-85,-350,-13);
    drawi(0,-350,42);
    drawi(85,-350,-13);
    drawi(106,-163,-29);
    drawi(95,3,-35);
    drawi(180,3,-59);
    drawi(175,160,-55);
    drawi(187,160,-55);
    drawi(192,3,-59);
    drawi(311,3,-70);
    drawi(317,3,-72);
    drawi(341,157,-67);
    drawi(316,157,-60);
    drawi(362,235,-58);
    drawi(618,564,-109);
    drawi(618,565,-115);
    drawi(618,625,-115);
    drawi(618,625,-104);
    drawi(618,669,-104);
    drawi(618,657,-114);
    drawi(618,713,-111);
    drawi(618,768,-110);
    movei(-128,-350,-83);
    drawi(-88,-350,-135);
    drawi(0,-350,-174);
    drawi(88,-350,-135);
    drawi(105,-163,-128);
    movei(-140,827,-66);
    drawi(-98,828,-65);
    drawi(0,828,-64);
    drawi(98,828,-65);
    drawi(89,667,-85);
    drawi(144,667,-84);
    movei(-142,450,-49);
    drawi(-119,536,-40);
    movei(-142,450,-49);
    drawi(-86,337,-40);
    drawi(-172,337,-55);
    drawi(-175,160,-55);
    drawi(-90,160,-43);
    drawi(0,160,-8);
    drawi(90,160,-43);
    drawi(175,160,-55);
    drawi(172,337,-55);
    drawi(179,450,-57);
    drawi(195,450,-59);
    drawi(195,536,-68);
    movei(-142,450,-49);
    drawi(0,450,-19);
    drawi(142,450,-49);
    movei(-142,557,-4);
    drawi(-139,722,-4);
    drawi(-209,904,220);
    drawi(-209,954,220);
    drawi(-143,831,-5);
    drawi(-148,929,-6);
    drawi(-149,972,8);
    drawi(-110,828,-12);
    movei(-144,667,-84);
    drawi(-140,827,-66);
    drawi(-144,972,-49);
    drawi(-106,972,-48);
    movei(-144,667,-84);
    drawi(-89,667,-85);
    movei(-147,827,-66);
    drawi(-144,667,-84);
    drawi(-162,704,-141);
    drawi(-158,833,-106);
    drawi(-140,827,-66);
    movei(-147,827,-66);
    drawi(-144,972,-49);
    drawi(-155,950,-74);
    drawi(-158,833,-106);
    drawi(-147,827,-66);
    drawi(-165,828,-57);
    drawi(-159,972,-43);
    drawi(-144,972,-49);
    movei(-149,515,-6);
    drawi(-142,557,-4);
    drawi(-211,828,220);
    drawi(-209,904,220);
    movei(-149,929,-6);
    drawi(-149,972,8);
    drawi(0,972,-4);
    movei(-157,829,-9);
    drawi(-149,929,-6);
    drawi(-214,999,219);
    drawi(-223,953,216);
    drawi(-157,829,-9);
    drawi(-162,722,-10);
    drawi(-158,557,-9);
    drawi(-149,515,-6);
    drawi(-214,809,219);
    drawi(-211,828,220);
    movei(-159,972,-43);
    drawi(-149,972,8);
    drawi(-175,828,-18);
    drawi(-157,829,-9);
    movei(-172,667,-54);
    drawi(-162,722,-10);
    drawi(-232,904,214);
    drawi(-223,953,216);
    movei(-173,667,-54);
    drawi(-168,536,-50);
    drawi(-180,536,-56);
    drawi(-179,450,-57);
    drawi(-142,450,-49);
    movei(-175,828,-18);
    drawi(-165,828,-57);
    drawi(-181,667,-75);
    drawi(-144,667,-84);
    movei(-175,828,-18);
    drawi(-172,667,-54);
    movei(-176,337,-96);
    drawi(-88,337,-107);
    drawi(-85,160,-121);
    drawi(-181,160,-105);
    drawi(-176,337,-96);
    drawi(-182,337,-100);
    drawi(-182,337,-170);
    drawi(-183,404,-152);
    drawi(-183,404,-101);
    drawi(-182,337,-100);
    drawi(-185,160,-109);
    drawi(-181,160,-105);
    drawi(-182,3,-94);
    drawi(-105,3,-129);
    movei(-179,450,-57);
    drawi(-172,337,-55);
    drawi(-184,337,-55);
    drawi(-187,160,-55);
    drawi(-175,160,-55);
    drawi(-180,3,-59);
    drawi(-95,3,-35);
    drawi(0,3,7);
    drawi(95,3,-35);
    drawi(90,160,-43);
    drawi(86,337,-40);
    drawi(172,337,-55);
    drawi(184,337,-55);
    drawi(187,160,-55);
    drawi(316,157,-60);
    movei(-183,667,-74);
    drawi(-173,667,-54);
    movei(-183,667,-74);
    drawi(-180,536,-56);
    drawi(-195,536,-68);
    drawi(-195,450,-59);
    drawi(-179,450,-57);
    movei(-184,160,-182);
    drawi(-182,337,-170);
    drawi(-208,337,-195);
    drawi(-202,404,-182);
    drawi(-183,404,-152);
    drawi(-197,536,-108);
    drawi(-197,536,-68);
    drawi(-183,404,-101);
    drawi(-184,401,-91);
    drawi(-176,337,-96);
    movei(-184,401,-91);
    drawi(-88,401,-102);
    drawi(-88,337,-107);
    drawi(0,337,-118);
    drawi(88,337,-107);
    drawi(88,401,-102);
    drawi(87,536,-93);
    drawi(89,667,-84);
    movei(-185,160,-109);
    drawi(-184,160,-182);
    drawi(-189,3,-181);
    drawi(-189,3,-98);
    drawi(-182,3,-94);
    drawi(-187,-163,-75);
    drawi(-105,-163,-128);
    movei(-187,-163,-75);
    drawi(-105,-163,-129);
    movei(-187,-163,-75);
    drawi(-106,-163,-29);
    movei(-187,-163,-75);
    drawi(-128,-350,-83);
    movei(-187,-163,-75);
    drawi(-180,3,-59);
    drawi(-192,3,-59);
    drawi(-187,160,-55);
    drawi(-316,157,-60);
    drawi(-311,3,-70);
    drawi(-192,3,-59);
    drawi(-199,-163,-75);
    drawi(-187,-163,-75);
    drawi(-196,-165,-84);
    drawi(-189,3,-98);
    drawi(-185,160,-109);
    movei(-195,450,-59);
    drawi(-184,337,-55);
    drawi(-316,326,-45);
    drawi(-316,432,-52);
    drawi(-316,537,-64);
    drawi(-316,667,-85);
    drawi(-316,535,-81);
    drawi(-316,404,-79);
    drawi(-316,326,-76);
    drawi(-317,157,-78);
    drawi(-312,3,-78);
    drawi(-302,-163,-82);
    drawi(-199,-163,-75);
    drawi(-197,-178,-84);
    drawi(-196,-165,-84);
    drawi(-198,-176,-183);
    drawi(-189,3,-181);
    drawi(-198,-178,-183);
    drawi(-197,-178,-84);
    drawi(-211,-322,-97);
    drawi(-198,-178,-183);
    drawi(-198,-176,-183);
    movei(-196,536,-84);
    drawi(-184,401,-91);
    movei(-196,536,-84);
    drawi(-196,667,-86);
    drawi(-89,667,-84);
    drawi(-87,536,-93);
    drawi(-196,536,-84);
    movei(-196,667,-85);
    drawi(-183,667,-74);
    movei(-196,667,-85);
    drawi(-195,536,-68);
    movei(-197,477,-166);
    drawi(-197,510,-151);
    drawi(-197,536,-108);
    drawi(-197,667,-92);
    drawi(-197,536,-68);
    drawi(-304,536,-68);
    drawi(-304,667,-92);
    drawi(-197,667,-92);
    movei(-202,404,-182);
    drawi(-197,477,-166);
    drawi(-304,477,-166);
    drawi(-298,404,-183);
    drawi(-202,404,-182);
    drawi(-197,510,-151);
    drawi(-304,510,-151);
    drawi(-298,404,-183);
    drawi(-291,337,-195);
    drawi(-208,337,-195);
    drawi(-216,160,-217);
    drawi(-184,160,-182);
    movei(-214,1000,219);
    drawi(-148,929,-6);
    movei(-214,1000,219);
    drawi(-209,954,220);
    movei(-216,-178,-200);
    drawi(-198,-178,-183);
    drawi(-301,-178,-184);
    drawi(-284,-178,-201);
    drawi(-216,-178,-200);
    drawi(-220,3,-215);
    drawi(-189,3,-181);
    movei(-220,3,-215);
    drawi(-216,160,-217);
    drawi(-285,160,-218);
    drawi(-278,3,-216);
    drawi(-220,3,-215);
    movei(-227,828,215);
    drawi(-158,557,-9);
    movei(-227,828,215);
    drawi(-214,809,219);
    movei(-232,904,214);
    drawi(-227,828,215);
    movei(-284,-178,-201);
    drawi(-278,3,-216);
    drawi(-310,3,-182);
    drawi(-301,-176,-184);
    drawi(-301,-178,-184);
    drawi(-291,-322,-97);
    drawi(-211,-322,-97);
    movei(-291,337,-195);
    drawi(-285,160,-218);
    drawi(-316,160,-183);
    drawi(-310,3,-182);
    drawi(-301,-178,-184);
    drawi(-302,-178,-85);
    drawi(-197,-178,-84);
    movei(-302,-163,-82);
    drawi(-301,-176,-184);
    drawi(-303,-163,-85);
    drawi(-302,-178,-85);
    drawi(-291,-322,-97);
    movei(-302,-178,-85);
    drawi(-302,-163,-82);
    drawi(-311,3,-70);
    drawi(-317,3,-72);
    drawi(-302,-163,-82);
    movei(-304,477,-166);
    drawi(-304,510,-151);
    drawi(-304,536,-108);
    drawi(-197,536,-108);
    movei(-304,536,-108);
    drawi(-304,536,-68);
    drawi(-318,404,-85);
    drawi(-318,404,-154);
    drawi(-298,404,-183);
    movei(-304,536,-108);
    drawi(-304,667,-92);
    movei(-312,3,-78);
    drawi(-310,3,-182);
    movei(-316,157,-60);
    drawi(-316,326,-45);
    drawi(-362,371,-54);
    drawi(-362,468,-60);
    drawi(-316,432,-52);
    movei(-317,157,-78);
    drawi(-316,160,-183);
    drawi(-318,337,-171);
    drawi(-291,337,-195);
    movei(-317,3,-72);
    drawi(-312,3,-78);
    movei(-318,337,-171);
    drawi(-316,326,-76);
    drawi(-362,372,-82);
    drawi(-362,441,-84);
    drawi(-362,562,-85);
    drawi(-316,535,-81);
    movei(-318,337,-171);
    drawi(-318,404,-154);
    drawi(-304,536,-108);
    movei(-318,404,-154);
    drawi(-316,404,-79);
    drawi(-362,442,-84);
    drawi(-362,562,-85);
    drawi(-362,682,-89);
    drawi(-316,667,-85);
    movei(-341,157,-67);
    drawi(-316,157,-60);
    drawi(-362,235,-58);
    drawi(-362,371,-54);
    drawi(-618,625,-104);
    drawi(-618,669,-104);
    drawi(-362,468,-60);
    drawi(-362,563,-70);
    drawi(-316,537,-64);
    movei(-341,157,-67);
    drawi(-317,157,-78);
    drawi(-362,235,-73);
    drawi(-362,372,-82);
    drawi(-362,442,-84);
    movei(-341,157,-67);
    drawi(-317,3,-72);
    movei(-362,205,-66);
    drawi(-341,157,-67);
    movei(-362,205,-66);
    drawi(-362,235,-58);
    drawi(-618,564,-109);
    drawi(-618,565,-115);
    drawi(-362,235,-73);
    drawi(-362,205,-66);
    drawi(-618,548,-114);
    drawi(-618,564,-109);
    drawi(-618,625,-104);
    drawi(-618,625,-115);
    drawi(-362,372,-82);
    movei(-362,563,-70);
    drawi(-362,682,-89);
    drawi(-618,768,-110);
    drawi(-618,713,-105);
    drawi(-362,563,-70);
    movei(-52,-611,-33);
    drawi(0,-611,51);
    drawi(52,-611,-33);
    drawi(0,-759,-54);
    drawi(54,-759,-82);
    drawi(59,-759,-97);
    drawi(60,-759,-115);
    drawi(76,-611,-101);
    drawi(76,-611,-83);
    drawi(59,-759,-97);
    drawi(0,-1000,-160);
    drawi(54,-759,-82);
    drawi(71,-611,-67);
    drawi(85,-350,-13);
    drawi(128,-350,-83);
    drawi(88,-350,-135);
    drawi(76,-611,-101);
    drawi(0,-611,-179);
    drawi(-76,-611,-101);
    drawi(-76,-611,-83);
    drawi(-71,-611,-67);
    drawi(-54,-759,-82);
    drawi(0,-759,-54);
    drawi(-52,-611,-33);
    movei(-60,-759,-115);
    drawi(0,-1000,-160);
    drawi(60,-759,-115);
    drawi(0,-759,-180);
    drawi(-60,-759,-115);
    movei(-618,548,-114);
    drawi(-618,565,-115);
    drawi(-618,625,-115);
    drawi(-618,657,-114);
    drawi(-362,441,-84);
    movei(-618,657,-114);
    drawi(-618,669,-104);
    drawi(-618,713,-105);
    drawi(-618,713,-111);
    drawi(-362,562,-85);
    movei(-618,657,-114);
    drawi(-618,713,-111);
    drawi(-618,768,-110);
    movei(0,-163,-162);
    drawi(105,-163,-129);
    movei(0,667,-84);
    drawi(89,667,-85);
    movei(142,557,-4);
    drawi(211,828,220);
    drawi(214,809,219);
    drawi(227,828,215);
    drawi(232,904,214);
    movei(144,972,-49);
    drawi(159,972,-43);
    movei(149,515,-6);
    drawi(214,809,219);
    movei(149,929,-6);
    drawi(214,999,219);
    drawi(223,953,216);
    movei(158,557,-9);
    drawi(227,828,215);
    movei(172,667,-54);
    drawi(181,667,-75);
    movei(182,337,-100);
    drawi(185,160,-109);
    drawi(184,160,-182);
    drawi(189,3,-181);
    drawi(198,-178,-183);
    drawi(211,-322,-97);
    movei(182,337,-170);
    drawi(182,337,-100);
    movei(182,337,-170);
    drawi(183,404,-152);
    drawi(183,404,-101);
    drawi(197,536,-68);
    drawi(197,667,-92);
    drawi(304,667,-92);
    drawi(304,536,-108);
    drawi(304,536,-68);
    drawi(304,667,-92);
    movei(182,337,-170);
    drawi(184,160,-182);
    drawi(216,160,-217);
    drawi(220,3,-215);
    drawi(278,3,-216);
    drawi(284,-178,-201);
    drawi(301,-178,-184);
    drawi(302,-178,-85);
    drawi(302,-163,-82);
    drawi(317,3,-72);
    drawi(312,3,-78);
    drawi(310,3,-182);
    drawi(316,160,-183);
    drawi(317,157,-78);
    drawi(362,235,-73);
    drawi(362,372,-82);
    drawi(362,441,-84);
    drawi(362,562,-85);
    drawi(316,535,-81);
    drawi(316,667,-85);
    movei(182,337,-170);
    drawi(208,337,-195);
    drawi(216,160,-217);
    drawi(285,160,-218);
    drawi(291,337,-195);
    drawi(298,404,-183);
    drawi(304,477,-166);
    drawi(304,510,-151);
    drawi(304,536,-108);
    drawi(318,404,-154);
    drawi(318,404,-85);
    drawi(304,536,-68);
    drawi(197,536,-68);
    drawi(197,536,-108);
    drawi(197,667,-92);
    movei(183,404,-152);
    drawi(197,536,-108);
    drawi(304,536,-108);
    movei(183,404,-152);
    drawi(202,404,-182);
    drawi(208,337,-195);
    drawi(291,337,-195);
    drawi(318,337,-171);
    drawi(318,404,-154);
    drawi(298,404,-183);
    drawi(304,510,-151);
    drawi(197,510,-151);
    drawi(197,536,-108);
    movei(184,337,-55);
    drawi(195,450,-59);
    movei(184,337,-55);
    drawi(316,326,-45);
    drawi(362,371,-54);
    drawi(618,625,-104);
    drawi(618,564,-109);
    drawi(618,548,-114);
    drawi(618,565,-115);
    drawi(362,235,-73);
    drawi(362,205,-66);
    drawi(618,548,-114);
    movei(189,3,-181);
    drawi(220,3,-215);
    drawi(216,-178,-200);
    drawi(284,-178,-201);
    movei(197,-178,-84);
    drawi(302,-178,-85);
    drawi(303,-163,-85);
    drawi(301,-176,-184);
    drawi(310,3,-182);
    drawi(278,3,-216);
    drawi(285,160,-218);
    drawi(316,160,-183);
    drawi(318,337,-171);
    drawi(316,326,-76);
    drawi(316,404,-79);
    drawi(316,535,-81);
    movei(197,477,-166);
    drawi(197,510,-151);
    drawi(202,404,-182);
    drawi(298,404,-183);
    movei(197,477,-166);
    drawi(202,404,-182);
    movei(197,477,-166);
    drawi(304,477,-166);
    movei(198,-178,-183);
    drawi(216,-178,-200);
    movei(198,-178,-183);
    drawi(301,-178,-184);
    drawi(310,3,-182);
    movei(209,904,220);
    drawi(211,828,220);
    movei(291,-322,-97);
    drawi(302,-178,-85);
    movei(316,326,-76);
    drawi(317,157,-78);
    movei(316,326,-76);
    drawi(362,372,-82);
    drawi(362,442,-84);
    drawi(362,562,-85);
    movei(316,404,-79);
    drawi(318,404,-154);
    movei(316,404,-79);
    drawi(362,442,-84);
    movei(362,372,-82);
    drawi(618,625,-115);
    drawi(618,657,-114);
}
