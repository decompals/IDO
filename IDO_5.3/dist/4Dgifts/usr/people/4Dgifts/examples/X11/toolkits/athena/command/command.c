/*
 *  command.c
 *
 *  Example program to show how to use Athena Command widget.
 *
 *  Ivan M. Hajadi, 18-Dec-90
 */


#include <X11/Intrinsic.h>	/* intrinsics definitions */
#include <X11/StringDefs.h>	/* standard name-string definitions */
#include <X11/Xaw/Command.h>	/* athena Command widget */


/* callback for command widgets */

void Push(Widget w, caddr_t client_data, caddr_t call_data)
{
    /* Push will be called everytime the button is pushed */
    
    printf("Hi you just pushed button widget !\n");
}


void main(unsigned int argc, char** argv)
{
    Widget toplevel;
    Widget button;
    char   *program;
    
    program = argv[0];

    /* initialize Xt toolkit */
    
    toplevel = XtInitialize(
			    program, /* application name */
			    "Demo",  /* application class */
			    NULL,    /* resource manager options */
			    0,       /* number of options */
			    &argc,   /* number of arguments */
			    argv     /* command line arguments */
			    );

    /* create command/button widget */
    
    button = XtCreateManagedWidget(
				   "button", /* widget instance name */
				   commandWidgetClass, /* widget class */
				   toplevel, /* parent widget */
				   NULL,     /* argument list */
				   0         /* argument list size */
				   );

    /* add the callback to be called when button is pushed */
    
    XtAddCallback(
		  button,	/* widget */
		  XtNcallback,  /* callback name */
		  Push,         /* callback procedure */
		  NULL          /* client data */
		  );
    
    XtRealizeWidget(toplevel);	/* instantiate it now */
    XtMainLoop();		/* loop for events */
}


