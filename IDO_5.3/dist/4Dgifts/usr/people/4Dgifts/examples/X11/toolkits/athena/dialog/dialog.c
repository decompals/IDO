/*
 *  dialog.c 
 *
 *  Example program to show how to use Athena Dialog widget.
 *
 *  Ivan M. Hajadi, Silicon Graphics Inc., 18-Dec-90
 */


#include <X11/Intrinsic.h>	/* intrinsics definitions */
#include <X11/StringDefs.h>	/* standard name-string definitions */
#include <X11/Xaw/Dialog.h>	/* athena Dialog widget */
#include <X11/Xaw/Command.h>	/* athena Command widget */

#define BUFSIZE 80

void Push(Widget button, caddr_t dialog, caddr_t call_data)
{
    /* get the character string in the text field */
    
    char *str = XawDialogGetValueString((Widget) dialog);
    printf("%s\n", str);
}


void main(unsigned int argc, char** argv)
{
    Widget toplevel;
    Widget button;
    Widget dialog;
    char   *program;

    Arg args[10];		/* widget resources */
    register int i;
    char buf[BUFSIZE];		/* dialog text field buffer */
    
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

    /* set the resource for dialog widget */

    buf[0] = '\0';		/* default text field is empty */
    i = 0;
    XtSetArg(args[i], XtNlabel, (XtArgVal) "Enter Something:");i++; /* label */
    XtSetArg(args[i], XtNvalue, (XtArgVal) buf); i++; /* text field */
    
    /* create dialog widget */
    
    dialog = XtCreateManagedWidget(
				   "dialog", /* widget instance name */
				   dialogWidgetClass, /* widget class */
				   toplevel, /* parent widget */
				   args,     /* argument list */
				   i         /* argument list size */
				   );
    
    /* set the resource for button widget */

    i = 0;
    XtSetArg(args[i], XtNlabel, (XtArgVal) "Show"); i++; /* label */

    /* add button to dialog */
    
    button = XtCreateManagedWidget(
				   "button", /* widget instance name */
				   commandWidgetClass, /* widget class */
				   dialog,   /* parent widget */
				   args,     /* argument list */
				   i         /* argument list size */
				   );

    /* add callback to button widget */
    
    XtAddCallback(
		  button,	/* widget */
		  XtNcallback,  /* callback name */
		  Push,         /* callback procedure */
		  (XtPointer) dialog	/* dialog widget */
		  );
    
    XtRealizeWidget(toplevel);	/* instantiate it now */
    XtMainLoop();		/* loop for events */
}


