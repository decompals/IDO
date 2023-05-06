/*
 *  label.c
 *
 *  Example program to show how to use Athena Label widget.
 *
 *  Ivan M. Hajadi, 18-Dec-90
 */


#include <X11/Intrinsic.h>	/* intrinsics definitions */
#include <X11/StringDefs.h>	/* standard name-string definitions */
#include <X11/Xaw/Label.h>	/* athena Label widget */
#include <stdio.h>

/* define command line option and resource file option */

static XrmOptionDescRec options[] =
{
    {"-label", "*label.label", XrmoptionSepArg, NULL}
};


int Syntax(char *program)
{
    fprintf(stderr, "Usage: label [-label <text>]");
    exit(0);
}


void main(unsigned int argc, char** argv)
{
    Widget toplevel;
    char   *program;
    
    program = argv[0];

    /* initialize Xt toolkit */
       
    toplevel = XtInitialize(
			    program, /* application name */
			    "Demo",  /* application class */
			    options, /* resource manager options */
			    XtNumber(options), /* number of options */
			    &argc,   /* number of arguments */
			    argv     /* command line arguments */
			    );

    if (argc != 1)
       Syntax(program);
    
    /* create label widget */
    
    XtCreateManagedWidget(
			  "label", /* widget instance name */
			  labelWidgetClass, /* widget class */
			  toplevel, /* parent widget */
			  NULL,     /* argument list */
			  0         /* argument list size */
			  );
    
    XtRealizeWidget(toplevel);	/* instantiate it now */
    XtMainLoop();		/* loop for events */
}


