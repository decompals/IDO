/*
 * Simple demo program to demonstrate selection convenience routines.
 * This is a simplified version of the overall demo.  This version only
 * transfers STRING data, using the XA_PRIMARY selection.
 *
 * Most of this demo is just concerned with the actual Motif and demo issues:
 *	* Creating andc maintaining the GUI state
 *	* Interactions between the demo and the text widget.
 *
 * The only code that is directly involved in the data transfer is:
 *	*) #include <Sgt/SelectSimple.h>
 *	*) A call to SgtGetSimpleString() in pasteCB()
 *	*) A call to SgtPostSimpleString() in postCB()
 *	*) A call to SgtUnpostSimple() in unPost()
 */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Sgt/SelectSimple.h>

static void indicateNotPosted();
static void indicatePosted();
static void loseCB();
static void pasteCB();
static void postCB();
static void textValueCB();
static void unPost();
static void unSelectCB();

Widget form, pastePB, selectPB, statusValW, statusValLabel,
       statusMsgW, statusMsgLabel, text, top, unSelectPB;

static char *fallback_resources[] = {
	"*background:		red",
	"*highlightOnEnter:	TRUE",
	"*useSchemes:		all",
	(char *) NULL
};

/* Colors to use to indicate selected state in the Select pushbutton */
	Pixel	selectBG, selectFG;

/*
 * This demo only deals with one kind of data transfer at a time.  The same
 * kind of data is sent or received, as requested.  The following variables
 * specify the data transfer being demonstrated.
 */

Atom	currentSelection       = XA_PRIMARY;
char   *currentTargetString    = "STRING";

Boolean	isPosted               = FALSE;	/* posted selected data */
Boolean	isSelected             = FALSE;	/* indicating selected data */

main(int argc, char **argv)
{
    XtAppContext  	ac;
    Arg			args[10];
    int			n=0;

    XtSetArg(args[n], XmNallowShellResize, True);              n++;
    top = XtAppInitialize(&ac, "Test", NULL, 0, &argc, argv,
	  fallback_resources, args, n);
    buildTheGUI ();		/* Set up the demo window, widgets, etc */
    XtRealizeWidget(top);
    n=0;			/* Save colors for later highlighting */
    XtSetArg(args[n], XtNbackground, &selectBG); n++;
    XtSetArg(args[n], XtNforeground, &selectFG); n++;
    XtGetValues(selectPB, args, n);
    XtAppMainLoop(ac);
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/*
 * The following section contains the demo's callback routines.  As with
 * most Xt and/or Motif programs, the real work is done in the callbacks.
 *
 * Some of these are where all of the dynamic interaction with the simple
 * selection routines happens.
 */

/*
 * Visually indicate that we do not have anything posted -- i.e. that we
 * do not own the selection.
 */
static void
indicateNotPosted() {
	Arg args[2]; int n=0;
	isSelected = isPosted = FALSE;
	XmTextSetString (statusValW, "");
	XtSetArg(args[n], XtNbackground, selectBG); n++;
	XtSetArg(args[n], XtNforeground, selectFG); n++;
	XtSetValues(selectPB, args, n);
}

/*
 * Visually indicate that we do have something posted -- i.e. that we
 * do own the selection.
 */
static void
indicatePosted() {
	Arg args[2]; int n=0;
	isSelected = isPosted = TRUE;
	XtSetArg(args[n], XtNbackground, selectFG); n++;
	XtSetArg(args[n], XtNforeground, selectBG); n++;
	XtSetValues(selectPB, args, n);
}

/*
 * loseCB() gets called when we lose ownership of the selection.  This
 * happens when:
 *	* another process takes ownership of the selection
 *	* we voluntarily relinquish the selection, such as when the user
 *	  is editting it.
 */
static void
loseCB() {
	indicateNotPosted();
}

/*
 * pasteCB() is called when the user activates the "paste" pushbutton.
 * We attempt to read whatever is currently selected.
 */
static void
pasteCB()
{	char		buf[256];
	XtPointer	data;
	unsigned long	length;

	if (SgtGetSimpleSTRING(top,currentSelection,&length,&data)) {
		sprintf (buf, "Get/Convert/Paste STRING operation succeeded");
		if (length) {
			XmTextSetString (text, data);
			XtFree (data);
		}
	} else {
		sprintf (buf, "Get/Convert/Paste STRING operation failed");
	};

	XmTextSetString (statusMsgW, buf);	/* Report operation's status */
}

/*
 * postCB() is called when the user activates the "post/select" pushbutton.
 * We make the contents of the text widget available.
 */

static void
postCB() {
	char *s;

    /* Set up for the operation */
	if (isPosted) unPost();
	SgtSetLoseSelectionCallback(top, currentSelection,
		(XtLoseSelectionProc)&loseCB);

    /* Post the new data */
	s = XmTextGetString(text);
	if (SgtPostSimpleSTRING(top,currentSelection,strlen(s),s)) {
	    XmTextSetString (statusValW, "(top text window)");
	    XmTextSetString (statusMsgW,
		"Post/select/cut operation succeeded for STRING");
	    if (!isSelected) { /* Add callback for edit -- must unselect */
		XtAddCallback(text, XmNvalueChangedCallback,textValueCB,NULL);
		indicatePosted();
	    }
	} else {
	    unSelectCB();
	    XmTextSetString (statusValW, "");
	    XmTextSetString (statusMsgW, "Post/select/cut operation failed");
	};
}

/*
 * textValueCB() is called whenever the text in the text widget changes.
 * That makes the data we have posted out of date, so we unselect the text.
 *
 * In a more conventional Xt program, one could just supply the latest data
 * when asked.  That is not possible with the convenience functions.
 */
static void
textValueCB() {
	unSelectCB();
}

/*
 * Simple routine to remove external manifestations.
 */
static void
unPost() {
	SgtSetLoseSelectionCallback(top, currentSelection, NULL);
	SgtUnpostSimple(top, currentSelection);
	isPosted = FALSE;
}

/*
 * unSelectCB() is called when we no longer want the selection.  This can
 * happen in several ways:
 *	* explicitly: the user activates the "Unselect" pushbutton
 *	* implicitly: the user changes the data in the text widget
 *	* implicitly: the user uses selection in the text widget.  In that
 *		      case, we let the widget take over.
 */
static void
unSelectCB() {
	if (!isSelected) return;

    /* Get rid of the external artifacts */
	XtRemoveCallback(text, XmNvalueChangedCallback,
		(XtCallbackProc)textValueCB, NULL);
	unPost();

    /* Indicate the data is no longer selected */
	indicateNotPosted();
	isSelected = FALSE;
}


/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/*                                                                            */
/* Building the GUI is relegated to this function because it isn't important  */
/* to the main point of this demo -- using the simplified selection routines. */
/*                                                                            */
/******************************************************************************/
/******************************************************************************/


#define STD_OFFSET	10

buildTheGUI () {
    Arg args[25];
    Widget children[256];
    int c=0, c2, n;

    /* This form holds the entire demo */
    n=0;
    XtSetArg(args[n], XmNfractionBase,     100);               n++;
    XtSetArg(args[n], XmNresizePolicy,     XmRESIZE_GROW);     n++;
    form = XmCreateForm(top, "form", args, n);

    /* Button to undo the selection*/
    n = 0;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM);      n++;
    XtSetArg(args[n], XmNbottomOffset,     STD_OFFSET);         n++;
    XtSetArg(args[n], XmNleftAttachment,   XmATTACH_FORM);      n++;
    XtSetArg(args[n], XmNleftOffset,       STD_OFFSET);         n++;
    XtSetArg(args[n], XmNrightAttachment,  XmATTACH_POSITION);  n++;
    XtSetArg(args[n], XmNrightPosition,    33);                 n++;
    children[c++] = unSelectPB = XmCreatePushButton(form, "Disown Selection", args, n);
    XtAddCallback(unSelectPB, XmNactivateCallback,
	(XtCallbackProc)unSelectCB, NULL);

    /* Button to select the text in the text widget */
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,        unSelectPB);         n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM);      n++;
    XtSetArg(args[n], XmNbottomOffset,     STD_OFFSET);         n++;
    XtSetArg(args[n], XmNleftAttachment,   XmATTACH_WIDGET);    n++;
    XtSetArg(args[n], XmNleftWidget,       unSelectPB);         n++;
    XtSetArg(args[n], XmNleftOffset,       STD_OFFSET);         n++;
    XtSetArg(args[n], XmNrightAttachment,  XmATTACH_POSITION);  n++;
    XtSetArg(args[n], XmNrightPosition,    67);                 n++;
    children[c++] = selectPB = XmCreatePushButton(form, "Post / Select / Cut", args, n);
    XtAddCallback(selectPB, XmNactivateCallback,
	(XtCallbackProc)postCB, NULL);

    /* Button to paste selected text into the text widget */
    n = 0;
    XtSetArg(args[n], XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget,        selectPB);           n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM);      n++;
    XtSetArg(args[n], XmNbottomOffset,     STD_OFFSET);         n++;
    XtSetArg(args[n], XmNleftAttachment,   XmATTACH_WIDGET);    n++;
    XtSetArg(args[n], XmNleftWidget,       selectPB);           n++;
    XtSetArg(args[n], XmNleftOffset,       STD_OFFSET);         n++;
    XtSetArg(args[n], XmNrightAttachment,  XmATTACH_FORM);      n++;
    XtSetArg(args[n], XmNrightOffset,      STD_OFFSET);         n++;
    children[c++] = pastePB = XmCreatePushButton(form, "Get / Convert / Paste", args, n);
    XtAddCallback(pastePB, XmNactivateCallback,
	(XtCallbackProc)pasteCB, NULL);

    /* Status report line */
    n = 0;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET);    n++;
    XtSetArg(args[n], XmNbottomWidget,     unSelectPB);         n++;
    XtSetArg(args[n], XmNbottomOffset,     STD_OFFSET);         n++;
    XtSetArg(args[n], XmNleftAttachment,   XmATTACH_FORM);      n++;
    XtSetArg(args[n], XmNleftOffset,       STD_OFFSET);         n++;
    XtSetArg(args[n], XmNhighlightOnEnter, FALSE);              n++;
    XtSetArg(args[n], XmNlabelString,
		      XmStringCreateSimple("Posted Value:"));   n++;
    children[c++] = statusValLabel
		  = XmCreateLabel(form, "statusValLabel", args, n);

    n = 0;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget,     statusValLabel);     n++;
    XtSetArg(args[n], XmNleftAttachment,   XmATTACH_WIDGET);    n++;
    XtSetArg(args[n], XmNleftWidget,       statusValLabel);     n++;
    XtSetArg(args[n], XmNrightAttachment,  XmATTACH_POSITION);  n++;
    XtSetArg(args[n], XmNrightPosition,    33);                 n++;
    XtSetArg(args[n], XmNcursorPositionVisible, FALSE);         n++;
    XtSetArg(args[n], XmNeditable,         FALSE);              n++;
    XtSetArg(args[n], XmNhighlightOnEnter, FALSE);              n++;
    children[c++] = statusValW = XmCreateTextField(form, "status", args, n);

    n = 0;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget,     statusValW);         n++;
    XtSetArg(args[n], XmNleftAttachment,   XmATTACH_WIDGET);    n++;
    XtSetArg(args[n], XmNleftWidget,       statusValW);         n++;
    XtSetArg(args[n], XmNleftOffset,       STD_OFFSET);         n++;
    XtSetArg(args[n], XmNhighlightOnEnter, FALSE);              n++;
    XtSetArg(args[n], XmNlabelString,
		      XmStringCreateSimple("Status Message:")); n++;
    children[c++] = statusMsgLabel
		  = XmCreateLabel(form, "statusMsgLabel", args, n);

    n = 0;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET); n++;
    XtSetArg(args[n], XmNbottomWidget,     statusMsgLabel);     n++;
    XtSetArg(args[n], XmNleftAttachment,   XmATTACH_WIDGET);    n++;
    XtSetArg(args[n], XmNleftWidget,       statusMsgLabel);     n++;
    XtSetArg(args[n], XmNrightAttachment,  XmATTACH_FORM);      n++;
    XtSetArg(args[n], XmNrightOffset,      STD_OFFSET);         n++;
    XtSetArg(args[n], XmNcursorPositionVisible, FALSE);         n++;
    XtSetArg(args[n], XmNeditable,         FALSE);              n++;
    XtSetArg(args[n], XmNhighlightOnEnter, FALSE);              n++;
    children[c++] = statusMsgW = XmCreateTextField(form, "status", args, n);

    /* Text widget to cut and paste text to/from */
    n=0;
    XtSetArg(args[n], XmNtopAttachment,    XmATTACH_FORM);     n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET);   n++;
    XtSetArg(args[n], XmNbottomWidget,     statusMsgW);        n++;

    XtSetArg(args[n], XmNbottomOffset,     STD_OFFSET);        n++;
    XtSetArg(args[n], XmNleftAttachment,   XmATTACH_FORM);     n++;
    XtSetArg(args[n], XmNrightAttachment,  XmATTACH_FORM);     n++;
    XtSetArg(args[n], XmNeditMode,         XmMULTI_LINE_EDIT); n++;
    XtSetArg(args[n], XmNrows,             5);                 n++;
    XtSetArg(args[n], XmNscrolledWindowMarginHeight, STD_OFFSET); n++;
    XtSetArg(args[n], XmNscrolledWindowMarginWidth,  STD_OFFSET); n++;
    text = XmCreateScrolledText(form, "text", args, n);
    XtManageChild(text);
    XtAddCallback(text, XmNvalueChangedCallback,
		(XtCallbackProc)textValueCB, NULL);

    /* Now start the merry-go-round turning */
    XtManageChildren(children, c);
    XtManageChild(form);
}
