/*
 *  box.c
 *
 *  Example program to show how to use Athena Box widget.
 *
 *  Ivan M. Hajadi, 21-Dec-90
 */


#include <X11/Intrinsic.h>	/* intrinsics definitions */
#include <X11/StringDefs.h>	/* standard name-string definitions */
#include <X11/Xaw/Command.h>	/* athena Command widget */
#include <X11/Xaw/Box.h>	/* athena Box widget */


/* callback for command widgets */

void Push(Widget w, caddr_t which_button, caddr_t call_data)
{
    /* Push will be called everytime the button is pushed, and */
    /* appropiate message will be printed */
    
    printf("%s\n", (char *) which_button);
}


void main(unsigned int argc, char** argv)
{
    Widget toplevel;
    Widget box;
    Widget button1, button2, button3;
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

    /* create box widget */

    box = XtCreateManagedWidget(
				"box", /* widget instance name */
				boxWidgetClass, /* widget class */
				toplevel, /* parent widget */
				NULL,     /* argument list */
				0         /* argument list size */
				);

    /* create three buttons to be managed by box widget */
    
    button1 = XtCreateManagedWidget(
				    "button1", /* widget instance name */
				    commandWidgetClass, /* widget class */
				    box,       /* parent widget */
				    NULL,      /* argument list */
				    0          /* argument list size */
				    );
    
    XtAddCallback(
		  button1,	/* widget */
		  XtNcallback,  /* callback name */
		  Push,         /* callback procedure */
		  (XtPointer) "This is button 1" /* client data */
		  );
    
    button2 = XtCreateManagedWidget(
				    "button2", /* widget instance name */
				    commandWidgetClass, /* widget class */
				    box,       /* parent widget */
				    NULL,      /* argument list */
				    0          /* argument list size */
				    );
    
    XtAddCallback(
		  button2,	/* widget */
		  XtNcallback,  /* callback name */
		  Push,         /* callback procedure */
		  (XtPointer) "This is button 2" /* client data */
		  );
    
    button3 = XtCreateManagedWidget(
				    "button3", /* widget instance name */
				    commandWidgetClass, /* widget class */
				    box,       /* parent widget */
				    NULL,      /* argument list */
				    0          /* argument list size */
				    );
    
    XtAddCallback(
		  button3,	/* widget */
		  XtNcallback,  /* callback name */
		  Push,         /* callback procedure */
		  (XtPointer) "This is button 3" /* client data */
		  );
    
    XtRealizeWidget(toplevel);	/* instantiate it now */
    XtMainLoop();		/* loop for events */
}


