/*
 *  	Bounce GL Video Input Window
 *	Screensaver edition
 *
 *	Author: Jeff Glover, jeffg@sgi.com
 */

#ident "$Revision: 1.9 $"

#include <stdio.h>
#include <getopt.h>
#include <sys/param.h>				/* for MAX() */
#include <svideo.h>
#include <gl/gl.h>
#include <gl/device.h>

/* #define DEBUG_PRINT_EVENT 			/* define for event printout */

#ifdef OUTPUT_TRACK
#define OPTIONS "srb:hcmod"
#else
#define OPTIONS "srb:hcmd"
#endif

#ifdef OUTPUT_TRACK
static char *USAGE = "\
       usage: vbounce [-sprhcmod] [-b <sound>]\n\
               -s: screen saver version\n\
               -r: resize on bounce (see also: -b)\n\
               -b <sound>: sound effect \"boink\" on bounce\n\
               -h: continuous hue change, modulo x position\n\
               -c: change source (1, 2, 3) on bounce\n\
               -m: mono/color on bounce\n\
               -o: output window tracks bouncing video center\n\
\n";
#else
static char *USAGE = "\
       usage: vbounce [-sprhcmod] [-b <sound>]\n\
               -s: screen saver version\n\
               -r: resize on bounce (see also: -b)\n\
               -b <sound>: sound effect \"boink\" on bounce\n\
               -h: continuous hue change, modulo x position\n\
               -c: change source (1, 2, 3) on bounce\n\
               -m: mono/color on bounce\n\
\n";
#endif /* OUTPUT_TRACK */

usage()
{
    fprintf(stderr, "%s", USAGE);
    exit(1);
}

char sound_name[2048];
short vect[2];
int x=300, y=300, deltaX=2, deltaY=1;
#ifdef OUTPUT_TRACK
int outSizeX, outSizeY;
#endif
int large_bounce=0, ssaver=0, pal_mode=0;
int output_track=0, mono=0;
int resize=0, hue_change=0, change_source=0, mono_color=0;
int dont_move=0;
int rightlimit, toplimit, sizeX, sizeY;
SVhandle V;
long buf[4];
long win;
long getvideoparam(long arg);

main(argc, argv)
int argc;
char **argv;
{
    int c;
    short val;
    short vp_val;

    sound_name[0] = '\0';

    while ((c = getopt(argc, argv, OPTIONS)) != -1) {
	switch (c) {
	case 's':
	    ssaver = 1;
	    break;
	case 'r':
	    resize = 1;
	    break;
	case 'b':
	    strcpy(sound_name, optarg);
	    break;
	case 'h':
	    hue_change = 1;
	    break;
	case 'c':
	    change_source = 1;
	    break;
	case 'm':
	    mono_color = 1;
	    break;
#ifdef OUTPUT_TRACK
	case 'o':
	    output_track = 1;
	    break;
#endif
	default:
	    usage();
	    break;
	}
    }

    srandom(time(0) % 231);
   
    /* Open video device */
    if ((V = svOpenVideo()) == NULL) {
	svPerror("open");
	exit(1);
    }

    pal_mode = (getvideoparam(SV_BROADCAST) == SV_PAL);

    if (pal_mode) {
	sizeX = SV_PAL_XMAX/2-4;
	sizeY = SV_PAL_YMAX/2-2;
#ifdef OUTPUT_TRACK
	outSizeX = SV_PAL_XMAX;
	outSizeY = SV_PAL_YMAX;
#endif
    } else {
	sizeX = SV_NTSC_XMAX/2-4;
	sizeY = SV_NTSC_YMAX/2-2;
#ifdef OUTPUT_TRACK
	outSizeX = SV_NTSC_XMAX;
	outSizeY = SV_NTSC_YMAX;
#endif
    }

    if (ssaver) {
	prefposition(0,1023,0,767);
    } else {
	prefposition(300,sizeX+300,300,sizeY+300);
    }
    noborder();
    win = winopen("vbounce");

    /* Associate video input with this window */
    if (svBindGLWindow(V, win, SV_IN_REPLACE) < 0) {	
	svPerror("bindwindow");
	exit(1);
    }

    size_window();

    if (ssaver) {
	svWindowOffset(V, x, y);

	if (svBindGLWindow(V, win, SV_IN_REPLACE) < 0) {	
	    svPerror("bindwindow");
	    exit(1);
	}
    }

    rightlimit = sizeX;
    toplimit = sizeY;

    qdevice(KEYBD);
    qdevice(VIDEO);

    /* null event loop */
    while (1) {
	if (qtest() != 0) {
	    int dev;
	    switch (dev = qread(&val)) {
	    case KEYBD:
		switch (val) {
		case 27:
		    exit(0);
		    break;
		default:
		    break;
		}
	    case WINQUIT:
	    case WINSHUT:
		exit(0);
		break;
	    case VIDEO:
		if (val == SvVideoStarted) {
#ifdef DEBUG_PRINT_EVENT
 		    fprintf(stderr, "\tVideo Started\n");
#endif
		} else if (val == SvVideoPreempted || (val == SvVideoBusy)
				|| (val == SvVideoStopped)) {
#ifdef DEBUG_PRINT_EVENT
		    if (val == SvVideoPreempted)
 			fprintf(stderr, "\tVideo Preempted\n");
                    else if (val == SvVideoBusy)
 			fprintf(stderr, "\tVideo Busy\n");
		    else
			fprintf(stderr, "\tVideo Stopped\n"); 
#endif
		    /* shutdown window and wait for video to become */
		    /* available again */
		    winclose(win);
		    while (1) {
			while (qread(&vp_val) != VIDEO)
			    ;
			if (vp_val == SvActiveAttribute) {
			    if (ssaver) {
				prefposition(0,1023,0,767);
			    } else {
				prefposition(300,sizeX+300,300,sizeY+300);
			    }
			    noborder();
			    win = winopen("vbounce");
			    if (svBindGLWindow(V, win, SV_IN_REPLACE) < 0) {
				winclose(win);
			    } else {
				qdevice(VIDEO);
				break;
			    }
			}
		    }
		} else if (val == SvActiveAttribute) {
#ifdef DEBUG_PRINT_EVENT
 		    fprintf(stderr, "\tVideo Available\n");
#endif
		    if (svBindGLWindow(V, win, SV_IN_REPLACE) < 0)
			dont_move = 1;
		    else
			dont_move = 0;
		} else if (val == SvEncodingAttribute) {
#ifdef DEBUG_PRINT_EVENT
 		    fprintf(stderr, "\tVideo Encoding Changed\n");
#endif
		} else if (val == SvFreezeAttribute) {
#ifdef DEBUG_PRINT_EVENT
 		    fprintf(stderr, "\tVideo Freeze\n");
#endif
		} else if (val == SvSourceAttribute) {
#ifdef DEBUG_PRINT_EVENT
 		    fprintf(stderr, "\tVideo Source Attribute\n");
#endif
		} else if (val == SvParamChangeAttribute) {
#ifdef DEBUG_PRINT_EVENT
 		    fprintf(stderr, "\tVideo Param Change Attribute\n");
#endif
		} else {
#ifdef DEBUG_PRINT_EVENT
 		    fprintf(stderr, "\tUnknown Video Event\n");
#endif
		}
		break;
	    default:
		break;
	    }
	}

	if (!dont_move) {
	    /* alter position, using delta */
	    /* windows in GL use lower left as origin */
	    move_window();

	    sginap(2);		/* take this out and watch the fireworks! */
				/* minimum of 2 since 1 can return immed. */

	    /* infrequently bounce a large amount, */
	    /* so it doesn't get into a rut */

	    if (random() % (1024*1024) < 128) {
		large_bounce = 1;
		deltaX *= 5;
	    }

	    if (hue_change && !pal_mode) {
		buf[0] = SV_HUE;
		buf[1] = x % 256;
		svSetParam(V, buf, 2);
	    }

	    x += deltaX;
	    if (x > 1024-rightlimit) {
		x -= deltaX;
		deltaX = - deltaX;
		bounce();
	    }
	    if (x < 0) {
		x -= deltaX;
		deltaX = - deltaX;
		bounce();
	    }
	    if (large_bounce) {
		large_bounce = 0;
		deltaX /= 5;
	    }

	    y += deltaY;
	    if (y > 768-toplimit) {
		y -= deltaY;
		deltaY = - deltaY;
		bounce();
	    }
	    if (y < 0) {
		y -= deltaY;
		deltaY = - deltaY;
		bounce();
	    }
	} else {
	    if (svBindGLWindow(V, win, SV_IN_REPLACE) < 0)
		dont_move = 1;
	    else
		dont_move = 0;
	}
    }
}

bounce()
{
    play_sound(sound_name);

    if (change_source) {
	buf[0] = SV_SOURCE;
	buf[1] = random() % 3;
	svSetParam(V, buf, 2);
    }

    if (mono_color) {
	buf[0] = SV_COLOR;
	buf[2] = SV_INPUT_BYPASS;
	if (mono) {
	    buf[1] = SV_DEFAULT_COLOR;
	    buf[3] = FALSE;
	} else {
	    buf[1] = SV_MONO;
	    buf[3] = TRUE;
	}
	mono = !mono;
	svSetParam(V, buf, 4);
    }
    
    if (resize) {
	if (pal_mode) {
	    sizeX = random() % (SV_PAL_XMAX/2);		/* no MAX() here */
	    sizeX = MAX(sizeX, 128);	/* side effects, needs to be here */
	    sizeY = (sizeX * 3) / 4;	/* maintain 4:3 aspect ratio */
	} else {
	    sizeX = random() % (SV_NTSC_XMAX/2);	/* no MAX() here */
	    sizeX = MAX(sizeX, 128);	/* side effects, needs to be here */
	    sizeY = (sizeX * 3) / 4;	/* maintain 4:3 aspect ratio */
	}
	rightlimit = sizeX;
	toplimit = sizeY;
	if (x > 1024-rightlimit)
	    x = 1024-rightlimit;
	if (y > 768-toplimit)
	    y = 768-toplimit;
	size_window();
	move_window();
    }
}

move_window()
{
    int outX, outY;

    if (!ssaver) {
	winmove(x,y);
    } else {
	svWindowOffset(V, x, y);

	if (svBindGLWindow(V, win, SV_IN_REPLACE) < 0) {	
	    svPerror("bindwindow");
	    exit(1);
	}
    }
#ifdef OUTPUT_TRACK	/* isn't working 8/5/92 */
    if (output_track) {
	/* output window is always SV_NTSC_XMAX * SV_NTSC_YMAX or
	 * SV_PAL_XMAX * SV_PAL_YMAX.  Never resized.
	 * So, to calculate it's "Position" relative to the input window.
	 */
	outX = x + sizeX/2 - outSizeX/2;
	if (outX < 0) outX = 0;
	else if (outX+outSizeX > 1024) outX = 1024-outSizeX;
	outY = y + sizeY/2 - outSizeY/2;
	if (outY < 0) outX = 0;
	else if (outY+outSizeY > 768) outY = 768-outSizeY;
	svOutputOffset(V, outX, outY);
    }
#endif
}

size_window()
{
    if (!ssaver) {
        prefsize(sizeX, sizeY);
	noborder();
        winconstraints();
    }

    svSetSize(V, sizeX, sizeY);

    /* rebind window so size takes effect */
    if (svBindGLWindow(V, win, SV_IN_REPLACE) < 0) {	
	svPerror("bindwindow");
	exit(1);
    }
}

static char play_cmd[2048+128];

play_sound(sound_name)
char *sound_name;
{
    if (sound_name[0] != '\0') {
	sprintf(play_cmd, "playaifc -s %s > /dev/null 2>&1 &", sound_name);
	system(play_cmd);
    }
}

long
getvideoparam(long arg)
{
    long pvbuf[2];

    pvbuf[0] = arg;
    if (svGetParam(V, pvbuf, 2) < 0)
	svPerror("svGetParam");
    return pvbuf[1];
}
