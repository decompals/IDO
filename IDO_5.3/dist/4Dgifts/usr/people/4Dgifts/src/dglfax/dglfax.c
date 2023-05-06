/*
 *	dglfax - 
 *		Post electronic fax on another machine's screen
 *	
 *	$Revision: 1.1 $
 */
#include <stdio.h>
#include <malloc.h>
#include <errno.h>
#include <sys/param.h>
#include <gl/gl.h>
#include <gl/device.h>

long X_OUTOFWAY, Y_OUTOFWAY;

char *myname;					/* my username		*/
char myhost[MAXHOSTNAMELEN];			/* my hostname		*/
char title[2*MAXHOSTNAMELEN];			/* window title		*/

short debug, dit;
long xsize,ysize;				/* size of both windows	*/
long xorigin,yorigin;				/* origin of from window*/
int wind_to, wind_from;				/* window id's		*/
int menu;					/* the popup menu id	*/

/* GETARG returns either the remainder of the current argument or
    the next argument if there is one.	*/
#define GETARG token[1] ? ctmp=token+1, token=" ", ctmp : \
	    --argc > 0 ? *++argv : (char *)usage()

main(argc,argv)
    int argc;
    char **argv;
{
    short run, dev,val;
    unsigned long *pixels;			/* the picture		*/
    int update=0;

    /* parse command line arguments	*/
    while (--argc > 0 && **++argv == '-') {
	char *token, *ctmp;
	for (token = argv[0] + 1; *token; token++) 
	switch (*token) {
	    case 'd':		/* debug mode		*/
		debug = 1;
		break;
	    case 'u':		/* set update time	*/
		update = atoi(GETARG);
		break;
	    default:
		fprintf (stderr,"dglfax: illegal option - '%c'\n", *token);
		usage ();
		/* NOTREACHED */
		break;
	}
    }
    if (argc != 1) usage();	/* sanity check				*/

    if (debug) foreground ();
    noborder ();				/* open up 'from' window */
    wind_from = winopen("dglfax");		/* with no border	*/
    getsize (&xsize,&ysize);			/* get its size		*/
    getorigin (&xorigin,&yorigin);		/* get its origin	*/
    if (debug) {
	printf ("origin = %d %d\n",xorigin,yorigin);
	sleep(2);
	getorigin (&xorigin,&yorigin);		/* get its origin	*/
	printf ("origin = %d %d\n",xorigin,yorigin);
    }
    X_OUTOFWAY = getgdesc(GD_XPMAX)+32;		/* move window off scrn	*/
    Y_OUTOFWAY = 0;				/* to right so invisible */
    winmove (X_OUTOFWAY,Y_OUTOFWAY);

    wind_to = dglopen (argv[0], DGLTSOCKET);
    if (wind_to < 0) dglerror(argv[0]);

    /* open up a "null" target window so if target machine is same
    	as source machine we dont screw up the display IDs of the 
	source area	*/
    myname = (char *)cuserid(NULL);		/* get username		*/
    gethostname (myhost,sizeof(myhost));	/* and hostname		*/
    prefposition(0,0,0,0);
    noborder();
    wind_to = winopen("dglfax");		/* open up 'to' window	*/

    /* dont read pixels until after we verify access to server */
    pixels = (unsigned long *)malloc(xsize*ysize*sizeof(pixels[0]));
    readpic(pixels);				/* leaves wind_to setup	*/

    winmove(xorigin,yorigin);			/* move it to right spot */
    prefsize(xsize,ysize);			/* and set its size	*/
    winconstraints();				/* and apply it		*/
    /* readpix changes title but it doesn't work during noborder mode	*/
    set_title("from");				/* until wintitle is fixed */

    ding_a_ling ();
    RGBmode();
    gconfig();

    qdevice(WINFREEZE);				/* queue up some devs	*/
    qdevice(WINTHAW);
    qdevice(WINQUIT);
    qdevice(WINSHUT);
    qdevice(RIGHTMOUSE);
    if (update > 0) {				/* queue a timer if needed */
	noise(TIMER0,update*60);
	qdevice(TIMER0);
    }
    qreset();					/* clear the queue	*/
    qenter(REDRAW,1);				/* enter in a REDRAW	*/

    dither(dit=0);
    rebuild_menu();
    run = 1;

    while (run) {				/* main loop		*/
	dev = qread(&val);			/* read an event	*/
	switch (dev) {
	    case RIGHTMOUSE:			/* popup menu		*/
		if (val) switch (dopup(menu)) {
		    case 1: qenter (TIMER0,1);		/* pull	*/
			    break;
		    case 2: dither (dit = !dit);	/* dither*/
			    rebuild_menu();
			    qenter(REDRAW,1);
			    break;
		    case 3: qenter (WINQUIT,1);		/* quit	*/
			    break;
		}
		break;
	    case WINFREEZE:			/* while frozen discard	*/
		if (debug) printf ("got FREEZE event\n");
		do {
		    dev=qread(&val);		/* all queue events	*/
		    if (debug) printf ("FROZEN: qread(%d)=%d\n",val,dev);
		} while (dev != WINTHAW);	/* except WINTHAW	*/
		qreset();			/* clear the queue	*/
		if (update) qenter(TIMER0,1);
		qenter(REDRAW,1);		/* enter in a REDRAW	*/
		break;
	    case TIMER0:			/* pull new copy of pic	*/
		if (debug) printf ("got TIMER0 event\n");
		readpic(pixels);		/* fall thru to redraw	*/
	    case REDRAW:
		if (debug) printf ("got REDRAW event\n");
		while (qtest() == REDRAW) qread(&val);	/* flush any more */
		reshapeviewport();		/* just in case		*/
		redraw(pixels);			/* redraw the picture	*/
		break;
	    case WINQUIT:
	    case WINSHUT:
		run = 0;
		break;
	}
    }
    gexit();
    dglclose(-1);
    exit (0);
}

usage ()
{
    fprintf(stderr,"Usage: dglfax [-u secs] host\n");
    exit (1);
}

set_title (msg)
    char *msg;
{
    sprintf(title,"dglfax %s %s@%s",msg,myname,myhost);
    wintitle(title);
}

ding_a_ling ()
{
    setbell (1);				/* short beep		*/
    ding();					/* 3 rings		*/
    sginap (30);				/* sleep 1/2 second	*/
    ding();					/* 3 rings		*/
}

ding()
{
    ringbell();
    gflush ();
    sginap(16);
    ringbell();
    gflush ();
    sginap(16);
    ringbell();
    gflush ();
}

rebuild_menu()
{
    if (menu) freepup(menu);			/* free existing popup	*/
    menu = defpup("dglfax %t|pull");		/* create a popup menu	*/
    addtopup (menu, dit ? "dither off" : "dither on");
    addtopup (menu,"quit",0);
}

readpic (pixels)
    unsigned long *pixels;
{
    set_title("<---");
    winset(wind_from);
    readdisplay (xorigin,yorigin,xorigin+xsize-1,yorigin+ysize-1, pixels, 0);
    winset(wind_to);
    set_title("from");
}

redraw (pixels)
    unsigned long *pixels;
{
    lrectwrite (0,0,xsize-1,ysize-1,pixels);
}

dglerror (host)
    char *host;
{
    fprintf (stderr,"error: dglopen failed on host %s\n",host);
    exit (1);
}
