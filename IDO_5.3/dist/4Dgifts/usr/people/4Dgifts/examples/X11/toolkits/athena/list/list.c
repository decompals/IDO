/*
 *  list.c 
 *
 *  Example program to show how to use Athena List widget.
 *
 *  Ivan M. Hajadi, Silicon Graphics Inc., 20-Dec-90
 */

#include <X11/Intrinsic.h>	/* intrinsics definitions */
#include <X11/StringDefs.h>	/* standard name-string definitions */
#include <X11/Xaw/List.h>	/* athena List widget */
#include <stdio.h>

#define BUFSIZE 80


int Syntax(char *program)
{
    fprintf(stderr, "Usage: %s string [string string ...]\n", program);
    exit(-1);
}


void Notify(Widget list, caddr_t client_data, caddr_t item)
{
    int index = ((XawListReturnStruct *) item)->list_index;
    char *string = ((XawListReturnStruct *) item)->string;
    
    /* highlight the selected item, and automatically dehighlight other */
    /* highlighted string(s) */
    
    XawListHighlight(list, index);

    /* print the item's string */
    printf("Item %d is %s\n", index, string);
}


void main(unsigned int argc, char** argv)
{
    Widget toplevel;
    Widget list;
    char   *program;

    Arg args[10];		/* widget resources */
    register int i;
    char **string_list;		/* text strings */
    
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

    if (argc < 2)
       Syntax(program);

    /* array of text strings that is to be displayed in the list */
    /* get them from command line */
    
    argv++;			/* skip the program name */
    string_list = argv;
    
    /* set the resource for list widget */

    i = 0;
    /* text strings */
    XtSetArg(args[i], XtNlist, (XtArgVal) string_list); i++; 
    /* list size */
    XtSetArg(args[i], XtNnumberStrings, (XtArgVal) (argc-1)); i++;
    
    
    /* create list widget */
    
    list = XtCreateManagedWidget(
				 "list",   /* widget instance name */
				 listWidgetClass, /* widget class */
				 toplevel, /* parent widget */
				 args,     /* argument list */
				 i         /* argument list size */
				 );

    /* add callback to list widget */
    
    XtAddCallback(
		  list,	/* widget */
		  XtNcallback,  /* callback name */
		  Notify,       /* callback procedure */
		  NULL		/* client data */
		  );
    
    XtRealizeWidget(toplevel);	/* instantiate it now */
    XtMainLoop();		/* loop for events */
}


