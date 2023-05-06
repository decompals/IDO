/*
 *  scrollbar.c
 *
 *  Example program to show how to use Athena Scrollbar widget. 
 *
 *  Ivan M. Hajadi, Silicon Graphics Inc., 18-Dec-90
 */

#include <stdio.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>	/* intrinsics definitions */
#include <X11/StringDefs.h>	/* standard name-string definitions */
#include <X11/Xaw/Scrollbar.h>	/* athena Scrollbar widget */

#define SCBAR_SIZE 200
#define THUMB_SIZE 40


/* define command line option and resource file option */

static XrmOptionDescRec options[] =
{
    {"-orientation", "*scrollbar.orientation", XrmoptionSepArg, NULL}
};


/*
 * ivan - for some reason i think the following callback names should
 *        be the other way around (?).
 */

/*
 * The XtNscrollProc callback is used for incremental scrolling and is
 * called by the NotifyScroll action.  The position argument is a signed
 * quantity and should be cast to an int when used.  Using the default
 * button bindings, button 1 returns a positive value, and button 3
 * returns a negative value.  In both cases, the magnitude of the value
 * is the distance of the pointer in pixels from the top (or left) of
 * the scrollbar.  The value will never be less than zero or greater than
 * the length of the scrollbar.
 */
 
void Scroll(Widget scrollbar,
	    caddr_t scbar_size_ptr,
	    caddr_t position /* int */)
{
    float pos = (float) abs((int) position);
    float scbar_size = *(float *) scbar_size_ptr;
    
    printf("Thumb position is %d pixels from the top (or left) of ",
	   (int) position);
    printf("the scrollbar.\n");

    /* make click moves the thumb to current pointer position */
    XawScrollbarSetThumb(scrollbar, pos/scbar_size, -1.0 /* unchanged */); 
}

/*
 * The XtNjumpProc callback is used to implement smooth scrolling and is
 * called by the NotifyThumb action.  percentptr specifies the thumb position.
 * With default button bindings, button 2 moves the thumb interactively, and
 * the XtNjumpProc is called on each new position of the pointer.
 */

void Jump(Widget w, caddr_t client_data, caddr_t percentptr /* float* */)
{
    printf("Thumb position is %d %% from the top (or left) of ",
	   (int) (100.0 * (*(float *) percentptr)));
    printf("the scrollbar.\n");
}



void Syntax(char *program)
{
    fprintf(stderr, "Usage: %s\n", program);
    exit(-1);
}

void main(unsigned int argc, char** argv)
{
    Widget toplevel;
    Widget scrollbar;
    float  scbar_size = (float) SCBAR_SIZE;
    char   *program;

    /* scrollbar widget arguments */
    static Arg args[] =
    {
	{XtNlength, (XtArgVal) SCBAR_SIZE},
	{XtNthickness, (XtArgVal) THUMB_SIZE},     
    };

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

    if (argc != 1)
       Syntax(program);

    /* create scrollbar widget */
    
    scrollbar = XtCreateManagedWidget(
				      "scrollbar", /* widget instance name */
				      scrollbarWidgetClass, /* widget class */
				      toplevel, /* parent widget */
				      args,     /* argument list */
				      XtNumber(args) /* argument list size */
				      );

    XtAddCallback(
		  scrollbar,	   /* widget */
		  XtNscrollProc,   /* callback name */
		  Scroll,          /* callback procedure */
		  (XtPointer) &scbar_size      /* scrollbar size pointer */
		  );

    XtAddCallback(
		  scrollbar,	/* widget */
		  XtNjumpProc,  /* callback name */
		  Jump,         /* callback procedure */
		  NULL          /* client data */
		  );

    /* set the initial position and size of scrollbar thumb */
    
    XawScrollbarSetThumb(
			scrollbar,
			0.5,	/* position of the top of the thumb is */
				/* half the length of the scrollbar */
			0.2	/* initial thumb size is two tenth of */
				/* the length of the scrollbar */
			);

    XtRealizeWidget(toplevel);	/* instantiate it now */
    XtMainLoop();		/* loop for events */
}


