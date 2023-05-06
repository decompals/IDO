/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2
*/ 
#ifdef REV_INFO
#ifndef lint
static char rcsid[] = "$RCSfile: hellomotif.c,v $ $Revision: 1.4 $ $Date: 1993/01/30 17:26:15 $"
#endif
#endif
/*
*  (c) Copyright 1989, DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS. */
#include <stdio.h>


#include <Xm/Xm.h>          /* Motif Toolkit */
#include <Mrm/MrmPublic.h>   /* Mrm */
#ifdef OVERLAY_DEMO
#include "../lib/sgi_visual.h"
#endif /* OVERLAY_DEMO */

static MrmHierarchy	s_MrmHierarchy;		/* MRM database hierarch id */
static char		*vec[]={"hellomotif.uid"};
						/* MRM database file list   */
static MrmCode		class ;

static void helloworld_button_activate();

static MrmCount		regnum = 1 ;
static MrmRegisterArg	regvec[] = {
	{"helloworld_button_activate",(caddr_t)helloworld_button_activate}
	};

Display  	*display;
XtAppContext  	app_context;

/*
 *  Main program
 */
int main(argc, argv)
     int    argc;
     String argv[];
{
     /*
     *  Declare the variables to contain the two widget ids
     */
    Widget toplevel, helloworldmain;
#ifdef OVERLAY_DEMO
    Arg arglist[10] ;
    int s;	/* to make it easy to build for any visual -- see below */
    int status;
#else
    Arg arglist[1] ;
#endif /* OVERLAY_DEMO */
    int n;

    /*
     *  Initialize the MRM
     */


    MrmInitialize ();

    XtToolkitInitialize();

    app_context = XtCreateApplicationContext();

    display = XtOpenDisplay(app_context, NULL, argv[0], "helloworldclass",
#ifdef __sgi
                            NULL, 0, &argc, argv);
#else /* OSF original */
                            NULL, 0, (Cardinal *)&argc, argv);
#endif /* __sgi */
    if (display == NULL) {
        fprintf(stderr, "%s:  Can't open display\n", argv[0]);
        exit(1);
    }

    n = 0;
#ifdef OVERLAY_DEMO
    /*
     * We have added one command line argument to give runtime control over
     * which visual is used.
     */
    if ( (argc==2) && (isdigit(*argv[1])) )
	sscanf(argv[1], "%i", &s);
    else
    	s=5;
    if (s>5) s=5;
    switch (s) {
	case 0: status = SG_getUnderlayArgs(display, NULL, arglist, &n); break;
	case 1: status = SG_getNormalArgs(display, NULL, arglist, &n); break;
	case 2: status = SG_getPopupArgs(display, NULL, arglist, &n); break;
	case 3: status = SG_getOverlayArgs(display, NULL, arglist, &n); break;
	case 4: status = SG_getOverlay2Args(display, NULL, arglist, &n); break;
	case 5: status = SG_getOverlay4Args(display, NULL, arglist, &n); break;
    }
    if (status < 0) exit (status);
#endif /* OVERLAY_DEMO */
    XtSetArg(arglist[n], XmNallowShellResize, True);  n++;
    toplevel = XtAppCreateShell(argv[0], NULL, applicationShellWidgetClass,
                              display, arglist, n);

    /*
     *  Define the Mrm.hierarchy (only 1 file)
     */

    if (MrmOpenHierarchy (1,			    /* number of files	    */
			vec, 			    /* files     	    */
			NULL,			    /* os_ext_list (null)   */
			&s_MrmHierarchy)	    /* ptr to returned id   */
			!= MrmSUCCESS) {
	printf ("can't open hierarchy\n");
     }

    /*
     * 	Register our callback routines so that the resource manager can 
     * 	resolve them at widget-creation time.
     */

    if (MrmRegisterNames (regvec, regnum)
			!= MrmSUCCESS)
			    printf("can't register names\n");

    /*
     *  Call MRM to fetch and create the pushbutton and its container
     */

    if (MrmFetchWidget (s_MrmHierarchy,
			"helloworld_main",
			toplevel,
			&helloworldmain,
			&class)
			!= MrmSUCCESS)
			    printf("can't fetch interface\n");

    /*
     *  Make the toplevel widget "manage" the main window (or whatever the
     *  the uil defines as the topmost widget).  This will
     *  cause it to be "realized" when the toplevel widget is "realized"
     */

    XtManageChild(helloworldmain);
    
    /*
     *  Realize the toplevel widget.  This will cause the entire "managed"
     *  widget hierarchy to be displayed
     */

    XtRealizeWidget(toplevel);

    /*
     *  Loop and process events
     */

    XtAppMainLoop(app_context);

    /* UNREACHABLE */
    return (0);
}

static void helloworld_button_activate( widget, tag, callback_data )
	Widget	widget;
	char    *tag;
	XmAnyCallbackStruct *callback_data;
{
    Arg arglist[2];

    static int call_count = 0;


    call_count += 1 ;
    switch ( call_count )
        {
        case 1:
            XtSetArg( arglist[0], XmNlabelString,
                XmStringLtoRCreate("Goodbye\nWorld!",""));
            XtSetArg( arglist[1], XmNx, 11 );  
            XtSetValues( widget, arglist, 2 );
            break ;
        case 2:
            exit(1);
            break ;
        }
}
