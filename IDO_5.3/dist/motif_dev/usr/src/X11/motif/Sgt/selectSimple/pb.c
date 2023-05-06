/*
 * Simple demo program to demonstrate selection convenience routines.
 *
 * Code added for the data interchange is:
 *	*) #include <Sgt/SelectSimple.h>
 *	*) A call to SgtGetSimpleString() in pasteCB()
 *	*) A call to SgtPostSimpleString() in postCB()
 *	*) A call to SgtUnpostSimple() in unPost()
 */

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Sgt/SelectSimple.h>

static void loseCB();
static	void buttonCB();

Widget	top, button;
Boolean	ownSelection = FALSE;

static char *fr[] = {			/* Fallback resources */
        "*button*fontList:   rock36",
        "*button*background:   red",
        (char *) NULL
};


main(int argc, char **argv)
{
    XtAppContext  	ac;
    Arg			args[10];
    int			n=0;

    XtSetArg(args[n], XmNallowShellResize, True); n++;
    top = XtAppInitialize(&ac, "Test", NULL, 0, &argc, argv, fr, args, n);

    n = 0;
    XtSetArg(args[n], XmNlabelString,
	XmStringCreateLocalized("Initial State"));  n++;
    button = XmCreatePushButton(top, "button", args, n);
    XtManageChild(button);
    XtAddCallback(button, XmNactivateCallback, (XtCallbackProc)buttonCB, NULL);

    SgtSetLoseSelectionCallback(top, XA_PRIMARY, (XtLoseSelectionProc)&loseCB);

    XtRealizeWidget(top);
    XtAppMainLoop(ac);
}

/*
 * loseCB() gets called when we lose ownership of the selection.
 */
static void
loseCB() {
	Arg args[10];
	int n=0;

	fprintf (stderr, "loseCB() called\n");
	ownSelection = FALSE;
	SgtSwitchBackground(button);
	XtSetArg(args[n], XmNlabelString,
		XmStringCreateLocalized("NO SELECTION"));   n++;
	XtSetValues(button, args, n);
}

/*
 * buttonCB() is called whenever the button is pushed.
 */
static void
buttonCB() {
	Arg		args[10];
	XtPointer       data;
	unsigned long   length;
	char		*msg;
	int		n=0;

	static char buf[26];	/* Cannot be on the stack!! */

	fprintf (stderr, "buttonCB() called, ownSelection=%d\n", ownSelection);

    /* If we already have the selection, disown it */
	if (ownSelection) {
		SgtUnpostSimple(top, XA_PRIMARY);
		return;
	}

    /* Get current selection, if any, and post the result */
	if (SgtGetSimpleSTRING(top, XA_PRIMARY, &length, &data)) {
		msg = length ? data : "Zero Length Data";
	} else {
	        msg = "\"Get\" Operation Failed";
	};

    /* Now post our version of the selection ourselves */
	sprintf (buf, "%s %0.20s\n", "DEMO:", msg);
	if (SgtPostSimpleSTRING(top, XA_PRIMARY, strlen(buf), buf)) {
		ownSelection = TRUE;
		SgtSwitchBackground(button);
	} else {
		msg = "\"Post\" Operation Failed";
	};

    /* Set the message in the pushbutton's label */
	n = 0;
	XtSetArg(args[n], XmNlabelString, XmStringCreateLocalized(msg));  n++;
       	XtSetValues(button, args, n);
}
