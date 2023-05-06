/*
 * Demo program to demonstrate the selection convenience routines.
 */


/*
 * TODO:
 *	* Ensure ATOM declarations and init are OK -- maybe a macro to do them?
 *	* Be sure that you only get API tracing unless you turn on library
 *	  debugging.  Be sure all callback split the debug info right.
 *	  (getproc does now.  done, lose, convert probably don't.
 *	* Indicate untested functions in their status messages
 *	* Do all of the "post" routines to match the "get" routines
					POST	GET
		BITMAP		
		COLUMN_NUMBER		OK	OK (Needs to do real columns)
		LINE_NUMBER		OK	OK (Needs to do real lines)
		PIXMAP		

 *
 *	* (Library)Debug printouts must check for type=XA_STRING before
 *	  trying to print a string.
 *
 *	* Add FILE and -- pop up fsb dialog for select.
 *		* SgtPost SimpleFile could use incremental to ensure it can
 *		  handle a long file.  Test with a small buffer size, then
 *		  make production have a large buffer size.  Perhaps try to
 *		  malloc one as big as needed.
 *		* SimpleFile	given filename, advertise both filename and file
 *		* SimpleFileOnly	(if useful)
 *		* SimpleFileNameOnly	(if useful)
 *	* Non-text data types  pop up a suitable dialog box when they are
 *	  active.  E.g. FILE could pop up an FSB, SGI_IMAGE an Glx dialog, ...
 *	* Move as many resources as make sense to the fallback resources
 *	* Add a label for each radio box
 *	* Pixmap things:
 *		* Save currentDialogID and currentDialogType -- use to pop
 *		  the dialog up and down as needed.
 *		* Should the pixmap dialog be a toplevel shell?
 *		* Catch the popup window being closed
 *		* better name on the pixmap popup
 *		* verify that the pixmap and window are at the same depth.
 *		* pop dialog up only when a pixmap radio button is live
 *		* better way to position the popup
 *		* make the popup be the right size for the pixmap
 *		* resize the popup if there is a new pixmap of different size
 *
 *	* Radio box to control which pushbutton to show: SGI, common, all-ICCM
 *	* Draw a bitmap.  Xmap will supply bitmaps.
 *	* Do we need standard data types by length (e.g. BIT8, BIT16, BIT32)?
 *	  If so, is this a TARGET or a TYPE?
 *	* Don't intend tp support INSERT_PROPERTY, INSERT_SELECTION at this
 *	  time.
 *	* There are callbacks for when the text widget gains and loses
 *	  primary selection.  This demo seems to turn them off.  Not sure
 *	  whether this is a bug in the demo or a bug in the widget.
 */

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/MwmUtil.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Sgt/SelectSimple.h>

static void debugToggleCB();
static void debugLibToggleCB();
static void indicateNotPosted();
static void indicatePosted();
static void insertTextToggleCB();
static void loseCB();
static void pixmapCB();
static void pasteCB();
static void postCB();
static void setSelectionCB();
static void setTargetCB();
static void textValueCB();
static void unPost();
static void unSelectCB();

#define RC_ROWS		9
#define STD_OFFSET	10

#define BACKGROUND_DATA		0
#define BITMAP_DATA		( 1 + BACKGROUND_DATA )
#define CHARACTER_POSITION_DATA	( 1 + BITMAP_DATA )
#define CLASS_DATA		( 1 + CHARACTER_POSITION_DATA )
#define CLIENT_WINDOW_DATA	( 1 + CLASS_DATA )
#define COLORMAP_DATA		( 1 + CLIENT_WINDOW_DATA )
#define COLUMN_NUMBER_DATA	( 1 + COLORMAP_DATA )
#define COMPOUND_TEXT_DATA	( 1 + COLUMN_NUMBER_DATA )
#define DRAWABLE_DATA		( 1 + COMPOUND_TEXT_DATA )
#define DELETE_DATA		( 1 + DRAWABLE_DATA )
#define FILE_NAME_DATA		( 1 + DELETE_DATA )
#define FOREGROUND_DATA		( 1 + FILE_NAME_DATA )
#define HOSTNAME_DATA		( 1 + FOREGROUND_DATA )
#define IP_ADDRESS_DATA		( 1 + HOSTNAME_DATA )
#define LENGTH_DATA		( 1 + IP_ADDRESS_DATA )
#define LINE_NUMBER_DATA	( 1 + LENGTH_DATA )
#define LIST_LENGTH_DATA	( 1 + LINE_NUMBER_DATA )
#define MODULE_DATA		( 1 + LIST_LENGTH_DATA )
#define MULTIPLE_DATA		( 1 + MODULE_DATA )
#define NAME_DATA		( 1 + MULTIPLE_DATA )
#define ODIF_DATA		( 1 + NAME_DATA )
#define OWNER_OS_DATA		( 1 + ODIF_DATA )
#define PIXMAP_DATA		( 1 + OWNER_OS_DATA )
#define PROCEDURE_DATA		( 1 + PIXMAP_DATA )
#define PROCESS_INTEGER_DATA	( 1 + PROCEDURE_DATA )
#define PROCESS_STRING_DATA	( 1 + PROCESS_INTEGER_DATA )
#define STRING_DATA		( 1 + PROCESS_STRING_DATA )
#define TARGETS_DATA		( 1 + STRING_DATA )
#define TASK_INTEGER_DATA	( 1 + TARGETS_DATA )
#define TASK_STRING_DATA	( 1 + TASK_INTEGER_DATA )
#define TEXT_DATA		( 1 + TASK_STRING_DATA )
#define TIMESTAMP_DATA		( 1 + TEXT_DATA )
#define USER_DATA		( 1 + TIMESTAMP_DATA )

Pixmap pixmapPM; GC pixmapGC; Widget pixmapShell, pixmapDA;

Widget		top, form, bigRC, optionFrame, optionRC, selFrame, selRC,
		statusValW, statusValLabel, statusMsgW, statusMsgLabel,
		text, typeFrame, typeRC;

Widget		debugTogglePB, debugLibTogglePB, insertTextTogglePB,
		pastePB, selectPB, unSelectPB;

Widget		primaryPB, secondaryPB, clipboardPB, nonstandardPB;

Widget		backgroundPB,
		bitmapPB,
		character_positionPB,
		classPB,
		client_windowPB,
		colormapPB,
		column_numberPB,
		compound_textPB,
		deletePB,
		drawablePB,
		filenamePB,
		foregroundPB,
		hostnamePB,
		ip_addressPB,
		lengthPB,
		line_numberPB,
		list_lengthPB,
		modulePB,
		multiplePB,
		namePB,
		odifPB,
		owner_osPB,
		pixmapPB,
		procedurePB,
		processIntegerPB,
		processStringPB,
		stringPB,
		targetsPB,
		textPB,
		taskIntegerPB,
		taskStringPB,
		timestampPB,
		userPB;

Atom		CLIPBOARD_ATOM,
		NONSTANDARD_ATOM;

Atom		CHARACTER_POSITION_ATOM,
		BACKGROUND_ATOM,
		BITMAP_ATOM,
		CLASS_ATOM,
		CLIENT_WINDOW_ATOM,
		COLORMAP_ATOM,
		COMPOUND_TEXT_ATOM,
		DELETE_ATOM,
		DRAWABLE_ATOM,
		FILE_NAME_ATOM,
		FOREGROUND_ATOM,
		HOSTNAME_ATOM,
		IP_ADDRESS_ATOM,
		LENGTH_ATOM,
		LIST_LENGTH_ATOM,
		MODULE_ATOM,
		NAME_ATOM,
		ODIF_ATOM,
		OWNER_OS_ATOM,
		PROCEDURE_ATOM,
		PROCESS_ATOM,
		PIXMAP_ATOM,
		STRING_ATOM,
		TARGETS_ATOM,
		TASK_ATOM,
		TEXT_ATOM,
		TIMESTAMP_ATOM,
		USER_ATOM;

static char *fallback_resources[] = {
	"test*fontList:		lucidasanstypewriter-bold-12",
	"*background:		red",
	"*highlightOnEnter:	TRUE",
	"*useSchemes:		all",
	(char *) NULL
};

/* Insert or Replace text in the text widget? */
	Boolean insertText	 = FALSE;

/* Colors to use to indicate selected state in the Select pushbutton */
	Pixel	selectBG, selectFG;

/*
 *
 * This demo only deals with one kind of data transfer at a time.  The same
 * kind of data is sent or received, as requested.  The following variables
 * keep track of the data state at any instant.
 *
 */

/* Set initial state: COMMON */
	Atom	currentSelection       = XA_PRIMARY;
	int	currentTarget      = STRING_DATA;
	char   *currentTargetString    = "STRING";	/* for status window */
	char   *statusMessage          = NULL;	/* Overriding status message */

/* Set initial state: GET */
	char   *currentGetStatus    = "";

/* Set initial state: POST */
	char   *currentPostedString    = "";	/* currently posted value */
	char   *currentPostedStatus    = "";
	Boolean	isPosted               = FALSE;	/* posted selected data */
	Boolean	isSelected             = FALSE;	/* indicating selected data */

main(int argc, char **argv)
{
    XtAppContext  	ac;
    Arg			args[100];
    Widget		children[100];
    int			c=0, n;

    /* Initialize this demo program */
    n = 0;
    XtSetArg(args[n], XmNallowShellResize, True);              n++;
    top = XtAppInitialize(&ac, "Test", NULL, 0, &argc, argv,
	  fallback_resources, args, n);
    /* Initialize any atoms we are going to use */
    CLIPBOARD_ATOM	= XmInternAtom(XtDisplay(top), "CLIPBOARD", FALSE);
    NONSTANDARD_ATOM	= XmInternAtom(XtDisplay(top), "NONSTANDARD", FALSE);

    BACKGROUND_ATOM	= XmInternAtom(XtDisplay(top), "BACKGROUND", FALSE);
    BITMAP_ATOM		= XmInternAtom(XtDisplay(top), "BITMAP", FALSE);
    CHARACTER_POSITION_ATOM = XmInternAtom(XtDisplay(top),
				"CHARACTER_POSITION", FALSE);
    CLASS_ATOM		= XmInternAtom(XtDisplay(top), "CLASS", FALSE);
    CLIENT_WINDOW_ATOM	= XmInternAtom(XtDisplay(top), "CLIENT_WINDOW", FALSE);
    COLORMAP_ATOM	= XmInternAtom(XtDisplay(top), "COLORMAP", FALSE);
    COMPOUND_TEXT_ATOM	= XmInternAtom(XtDisplay(top), "COMPOUND_TEXT", FALSE);
    DELETE_ATOM		= XmInternAtom(XtDisplay(top), "DELETE", FALSE);
    DRAWABLE_ATOM	= XmInternAtom(XtDisplay(top), "DRAWABLE", FALSE);
    FILE_NAME_ATOM	= XmInternAtom(XtDisplay(top), "FILE_NAME", FALSE);
    FOREGROUND_ATOM	= XmInternAtom(XtDisplay(top), "FOREGROUND", FALSE);
    HOSTNAME_ATOM	= XmInternAtom(XtDisplay(top), "HOSTNAME", FALSE);
    IP_ADDRESS_ATOM	= XmInternAtom(XtDisplay(top), "IP_ADDRESS", FALSE);
    LENGTH_ATOM		= XmInternAtom(XtDisplay(top), "LENGTH", FALSE);
    LIST_LENGTH_ATOM	= XmInternAtom(XtDisplay(top), "LIST_LENGTH", FALSE);
    MODULE_ATOM		= XmInternAtom(XtDisplay(top), "MODULE", FALSE);
    NAME_ATOM		= XmInternAtom(XtDisplay(top), "NAME", FALSE);
    ODIF_ATOM		= XmInternAtom(XtDisplay(top), "ODIF", FALSE);
    OWNER_OS_ATOM	= XmInternAtom(XtDisplay(top), "OWNER_OS", FALSE);
    PIXMAP_ATOM		= XmInternAtom(XtDisplay(top), "PIXMAP", FALSE);
    PROCEDURE_ATOM	= XmInternAtom(XtDisplay(top), "PROCEDURE", FALSE);
    PROCESS_ATOM	= XmInternAtom(XtDisplay(top), "PROCESS", FALSE);
    STRING_ATOM		= XmInternAtom(XtDisplay(top), "STRING", FALSE);
    TARGETS_ATOM	= XmInternAtom(XtDisplay(top), "TARGETS", FALSE);
    TASK_ATOM		= XmInternAtom(XtDisplay(top), "TASK", FALSE);
    TEXT_ATOM		= XmInternAtom(XtDisplay(top), "TEXT", FALSE);
    TIMESTAMP_ATOM	= XmInternAtom(XtDisplay(top), "TIMESTAMP", FALSE);
    USER_ATOM		= XmInternAtom(XtDisplay(top), "USER", FALSE);

    /* Build the demo GUI */
    buildTheGUI ();

    /* Final initialization */
    XtRealizeWidget(top);
    n=0;			/* Save colors for later highlighting */
    XtSetArg(args[n], XtNbackground, &selectBG); n++;
    XtSetArg(args[n], XtNforeground, &selectFG); n++;
    XtGetValues(selectPB, args, n);

    /* Now start the merry-go-round turning */
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
 * This is also where all of the dynamic interaction with the simple
 * selection routines happens.
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
{	char		buf[BUFSIZ], *s, *ss, *sss;
	XtPointer	data;
	int		i;
	unsigned long	l, length;
	Boolean		status;

	switch (currentTarget) {
	    case BACKGROUND_DATA:
		ss = "BACKGROUND";
		status = SgtGetSimpleBACKGROUND(top,currentSelection,&length,&data);
		break;
	    case BITMAP_DATA:
		ss = "BITMAP";
		status = SgtGetSimpleBITMAP(top,currentSelection,&length,&data);
		break;
	    case CHARACTER_POSITION_DATA:
		ss = "CHARACTER_POSITION";
		status = SgtGetSimpleCHARACTER_POSITION(top,currentSelection,&length,&data);
		break;
	    case CLASS_DATA:
		ss = "CLASS";
		status = SgtGetSimpleCLASS(top,currentSelection,&length,&data);
		break;
	    case CLIENT_WINDOW_DATA:
		ss = "CLIENT_WINDOW";
		status = SgtGetSimpleCLIENT_WINDOW(top,currentSelection,&length,&data);
		break;
	    case COLORMAP_DATA:
		ss = "COLORMAP";
		status = SgtGetSimpleCOLORMAP(top,currentSelection,&length,&data);
		break;
	    case COLUMN_NUMBER_DATA:
		ss = "COLUMN_NUMBER";
		status = SgtGetSimpleCOLUMN_NUMBER(top,currentSelection,&length,&data);
		break;
	    case COMPOUND_TEXT_DATA:
		ss = "COMPOUND_TEXT";
		status = SgtGetSimpleCOMPOUND_TEXT(top,currentSelection,&length,&data);
		break;
	    case DELETE_DATA:
		ss = "DELETE";
		status = SgtGetSimpleDELETE(top,currentSelection,&length,&data);
		break;
	    case DRAWABLE_DATA:
		ss = "DRAWABLE";
		status = SgtGetSimpleDRAWABLE(top,currentSelection,&length,&data);
		break;
	    case FILE_NAME_DATA:
		ss = "FILE_NAME";
		status = SgtGetSimpleFILE_NAME(top,currentSelection,&length,&data);
		break;
	    case FOREGROUND_DATA:
		ss = "FOREGROUND";
		status = SgtGetSimpleFOREGROUND(top,currentSelection,&length,&data);
		break;
	    case HOSTNAME_DATA:
		ss = "HOSTNAME";
		status = SgtGetSimpleHOSTNAME(top,currentSelection,&length,&data);
		break;
	    case IP_ADDRESS_DATA:
		ss = "IP_ADDRESS";
		status = SgtGetSimpleIP_ADDRESS(top,currentSelection,&length,&data);
		break;
	    case LENGTH_DATA:
		ss = "LENGTH";
		status = SgtGetSimpleLENGTH(top,currentSelection,&length,&data);
		break;
	    case LINE_NUMBER_DATA:
		ss = "LINE_NUMBER";
		status = SgtGetSimpleLINE_NUMBER(top,currentSelection,&length,&data);
		break;
	    case LIST_LENGTH_DATA:
		ss = "LIST_LENGTH";
		status = SgtGetSimpleLIST_LENGTH(top,currentSelection,&length,&data);
		break;
	    case MODULE_DATA:
		ss = "MODULE";
		status = SgtGetSimpleMODULE(top,currentSelection,&length,&data);
		break;
	    case NAME_DATA:
		ss = "NAME";
		status = SgtGetSimpleNAME(top,currentSelection,&length,&data);
		break;
	    case ODIF_DATA:
		ss = "ODIF";
		status = SgtGetSimpleODIF(top,currentSelection,&length,&data);
		break;
	    case OWNER_OS_DATA:
		ss = "OWNER_OS";
		status = SgtGetSimpleOWNER_OS(top,currentSelection,&length,&data);
		break;
	    case PIXMAP_DATA:
		ss = "PIXMAP";
		status = SgtGetSimplePIXMAP(top,currentSelection,&length,&data);
		break;
	    case PROCEDURE_DATA:
		ss = "PROCEDURE";
		status = SgtGetSimplePROCEDURE(top,currentSelection,&length,&data);
		break;
	    case PROCESS_INTEGER_DATA:
		ss = "PROCESS_INTEGER";
		status = SgtGetSimplePROCESS_INTEGER(top,currentSelection,&length,&data);
		break;
	    case PROCESS_STRING_DATA:
		ss = "PROCESS_STRING";
		status = SgtGetSimplePROCESS_STRING(top,currentSelection,&length,&data);
		break;
	    case STRING_DATA:
		ss = "STRING";
		status = SgtGetSimpleSTRING(top,currentSelection,&length,&data);
		break;
	    case TARGETS_DATA:
		ss = "TARGETS";
		status =SgtGetSimpleTARGETS(top,currentSelection,&length,&data);
		break;
	    case TASK_INTEGER_DATA:
		ss = "TASK_INTEGER";
		status = SgtGetSimpleTASK_INTEGER(top,currentSelection,&length,&data);
		break;
	    case TASK_STRING_DATA:
		ss = "TASK_STRING";
		status = SgtGetSimpleTASK_STRING(top,currentSelection,&length,&data);
		break;
	    case TEXT_DATA:
		ss = "TEXT";
		status = SgtGetSimpleTEXT(top,currentSelection,&length,&data);
		break;
	    case TIMESTAMP_DATA:
		ss = "TIMESTAMP";
		status = SgtGetSimpleTIMESTAMP(top,currentSelection,&length,&data);
		break;
	    case USER_DATA:
		ss = "USER";
		status = SgtGetSimpleUSER(top,currentSelection,&length,&data);
		break;
	    case MULTIPLE_DATA:
		XmTextSetString (statusMsgW,
"A convenience routine is not appropriate for getting \"MULTIPLE\"");
		return;

	    default:
		XmTextSetString (statusMsgW, "This demo does no know how to paste this data type");
		return;
	}

    /* No reporting to do if the "get" failed */
	if (! status) {
		char buf[256];
		sprintf (buf, "Get/Convert/Paste %s operation failed", ss);
		XmTextSetString (statusMsgW, buf);
		return;
	};
	
    /* Report the data we got */
	{ char buf[256];
	  sprintf (buf, "Get/Convert/Paste %s operation succeeded", ss);
	  XmTextSetString (statusMsgW, buf);
	}
	switch (currentTarget) {

	  /* Text data types */
	    case CLASS_DATA:
	    case FILE_NAME_DATA:
	    case HOSTNAME_DATA:
	    case MODULE_DATA:
	    case NAME_DATA:
	    case ODIF_DATA:
	    case OWNER_OS_DATA:
	    case PROCEDURE_DATA:
	    case PROCESS_STRING_DATA:
	    case STRING_DATA:
	    case TASK_STRING_DATA:
	    case TEXT_DATA:
	    case USER_DATA:
		if (length) {
			if (insertText)
				XmTextInsert(text,
					     XmTextGetInsertionPosition(text),
					     data);
			else
				XmTextSetString (text, data);
		}
		break;

	  /* INTEGER data types */
	    case LENGTH_DATA:
	    case LIST_LENGTH_DATA:
	    case PROCESS_INTEGER_DATA:
	    case TASK_INTEGER_DATA:
	    case TIMESTAMP_DATA:
		sprintf (buf, "%d\0", *(int*)data);
		XmTextSetString (text, buf);
		break;

	  /* SPAN data types */
	    case CHARACTER_POSITION_DATA:
	    case COLUMN_NUMBER_DATA:
	    case LINE_NUMBER_DATA:
		sprintf (buf, "%d, %d\0", *((int*)data), *((int*)(data)+1));
		XmTextSetString (text, buf);
		break;

	    /*
	     * The following are printed in hex, to match other utilities.
	     * Each is a list, except CLIENT_WINDOW, whose length is 1 and
	     * so works anyway.  Each of the data types is 32 bits, so we
	     * fake it by casting to an int.  We'll fix that in the future,
	     * if it ever becomes necessary.
	     */
	    case BACKGROUND_DATA:
	    case BITMAP_DATA:
	    case CLIENT_WINDOW_DATA:
	    case COLORMAP_DATA:
	    case DRAWABLE_DATA:
	    case FOREGROUND_DATA:
	    case PIXMAP_DATA:
		sprintf(buf,"Length = %d\0", length);
		for (i=0; i<length; i++) {
			sprintf (buf+strlen(buf), "\n0x%x\0", *(int*)data);
		}
		XmTextSetString (text, buf);

		/* Now draw the data for the kinds we know how to draw */
		if (currentTarget== PIXMAP_DATA) {
			Arg args[10];
			int n;

			/* If there was a pre-existing one, update the data */
			if (pixmapShell) {
				XFreePixmap(XtDisplay(pixmapShell),pixmapPM);
				XtRemoveCallback(pixmapDA, XmNexposeCallback,
					(XtCallbackProc)pixmapCB,
					(XtPointer)pixmapPM);
				pixmapPM = *(Drawable*)data;
				XtAddCallback(pixmapDA, XmNexposeCallback,
					(XtCallbackProc)pixmapCB,
					(XtPointer)pixmapPM);
				pixmapCB(pixmapDA, pixmapPM);
				break;
			}

			/* Was no old one -- create a new one. */
			n = 0;
			XtSetArg(args[n], XmNwidth,  400); n++;
			XtSetArg(args[n], XmNheight, 400); n++;
			XtSetArg(args[n], XmNx, 100);      n++;
			XtSetArg(args[n], XmNy, 200);      n++;
			XtSetArg(args[n], XmNdeleteResponse, XmUNMAP); n++;
			XtSetArg(args[n], XmNmwmDecorations,
				MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE); n++;
			XtSetArg(args[n], XmNmwmFunctions,
				MWM_FUNC_ALL | MWM_FUNC_MAXIMIZE); n++;
			pixmapShell = XmCreateDialogShell
					(top, "Pixmap View", args, n);
			pixmapDA = XmCreateDrawingArea
					(pixmapShell, "drawingArea", args, n);
			pixmapPM = *(Drawable*)data;
			XtAddCallback(pixmapDA, XmNexposeCallback,
				(XtCallbackProc)pixmapCB, (XtPointer)pixmapPM);

			XtManageChild(pixmapDA);
			XtRealizeWidget(pixmapShell);
			pixmapGC = XCreateGC(XtDisplay(pixmapDA), pixmapPM,0,0);
			XtPopup(pixmapShell, XtGrabNone);
		}
		break;

	    /* We need to convert the compound text to a normal C string
	     * to give it to the text widget.  Doing so is, of course,
	     * simplistic.  In practice, however, it is fine for this demo
	     * program.  COMPOUND_TEXT -> XmSTRING -> TEXT.
	     */
	    case COMPOUND_TEXT_DATA:
		{ char *s;
		  if (XmStringGetLtoR( XmCvtCTToXmString(data),
		    XmFONTLIST_DEFAULT_TAG, &s)) {
			if (insertText)
				XmTextInsert(text,
				     XmTextGetInsertionPosition(text), data);
			else
				XmTextSetString (text, data);
			XtFree(s);
		  } else {
			XmTextSetString (statusMsgW,
				"Could not convert the compound text");
		  }
		}
		break;
	    /* Converting DELETE returns nothing; the other end deletes */
	    case DELETE_DATA:
		break;

	    /* A NET_ADDRESS is 4 bytes */
	    case IP_ADDRESS_DATA:
		sprintf (buf, "%d.%d.%d.%d\0",
			*(((char*)data)),   *(((char*)data)+1),
			*(((char*)data)+2), *(((char*)data)+3));
		XmTextSetString (text, buf);
		break;

	    case TARGETS_DATA:
		sprintf(buf,"Length = %d\0", length);
		for (i=0; i<length; i++) {
			s = XGetAtomName(XtDisplay(top),((Atom*)data)[i]);
			sprintf (buf+strlen(buf), "\n(%d) %s\0", i, s);
		}
		XmTextSetString (text, buf);
	}
	XtFree (data);
}


/*
 * If were trying to be efficient, we'd just refresh the newly exposed
 * area.  But since this is a demo, we just do the whole window, whether
 * or not we need to.
 */
static void
pixmapCB(Widget w, Drawable dp) {
	XCopyArea(XtDisplay(w), dp, XtWindow(pixmapDA), pixmapGC, 0, 0, 400, 400, 0, 0);
}


/*
 * postCB() is called when the user activates the "post/select" pushbutton.
 * We make the contents of the text widget available.  There are two cases:
 *	* we may be making data available for the first time.
 *	* we may be updating data we had previously made available, such
 *	  as changing its type or selection.  (If the data itself had
 *	  changed, we would have already unselected it.)
 */

static void
postCB() {
	XmString xs;
	char *s, *ss="", valueString[80];
	XmTextPosition left, right;
	Boolean status;

    /* Post our new data */
	if (isPosted)
		unPost();	/* If anything is outstanding, cancel it */
	SgtSetLoseSelectionCallback(top, currentSelection,
		(XtLoseSelectionProc)&loseCB);

    /*
     * Many things work with the string in the text widget.  We use the
     * selected text for CHARACTER_POSITION, COLUMN_NUMBER, and LINE_NUMBER.
     */
	s = XmTextGetString(text);
	if (XmTextGetSelectionPosition(text, &left, &right))
		left++;
	else
		left = right = 0;

    /* Now handle each data type individually */
	switch (currentTarget) {

	    /* This just gets the background from the text widget */
	    case BACKGROUND_DATA:
	    {	Arg args[10]; int n=0; static Pixel bg;
		ss = "(top text widget background used)";
		XtSetArg(args[n], XtNbackground, &bg); n++;
		XtGetValues(text, args, n);
		sprintf (valueString, "%d", bg);
		status = SgtPostSimpleBACKGROUND(text,currentSelection,1,&bg);
		break;
	    }

	    case CHARACTER_POSITION_DATA:
	    {	long *temp = (long *) XtMalloc (2 * sizeof(long));
		ss = "(selected characters in top text widget)";
		temp[0] = left;
		temp[1] = right;
		sprintf (valueString, "%d, %d", temp[0], temp[1]);
		status = SgtPostSimpleCHARACTER_POSITION(
				top,currentSelection,2,temp);
		break;
	    }

	    case COLORMAP_DATA:
	    {	Arg args[10]; int n=0; static Colormap cm;
		ss = "(shell's colormap)";
		XtSetArg(args[n], XtNcolormap, &cm); n++;
		XtGetValues(top, args, n);
		sprintf (valueString, "0x%x", cm);
		status = SgtPostSimpleCOLORMAP(top,currentSelection,1,&cm);
		break;
	    }

	    case COLUMN_NUMBER_DATA:
	    {	long *temp = (long *) XtMalloc (2 * sizeof(long));
		ss = "(selected characters in top text widget)";
		temp[0] = left;
		temp[1] = right;
		ss = "(selected characters in top text widget)";
		sprintf (valueString, "%d, %d", temp[0], temp[1]);
		status = SgtPostSimpleCOLUMN_NUMBER(top,currentSelection,2,temp);
		break;
	    }

	    case COMPOUND_TEXT_DATA:
		xs = XmStringCreateSimple(s);
		sprintf (valueString, "(top text window)");
		status = SgtPostSimpleCOMPOUND_TEXT(top,currentSelection,
				XmStringLength(xs),xs);
		break;

	    case DELETE_DATA:
		XmTextSetString (statusMsgW,
		    "\"DELETE\" is never posted.  It is a side-effect target.");
		XmTextSetString (statusValW, "");
		return;

	    case DRAWABLE_DATA:
	    {	static Window win;
		ss = "(shell's window)";
		win = XtWindow(top);
		sprintf (valueString, "0x%x", win);
		status = SgtPostSimpleDRAWABLE(top,currentSelection, 1, &win);
		break;
	    }

	    case FILE_NAME_DATA:
	    {	static char *s = "FakeFileName";
		sprintf (valueString, "%s", s);
		status = SgtPostSimpleFILE_NAME(top,currentSelection,
				strlen(s),s);
		break;
	    }

	    /* This just gets the foreground from the text widget */
	    case FOREGROUND_DATA:
	    {	Arg args[10]; int n=0; static Pixel fg;
		ss = "(top text widget foreground used)";
		XtSetArg(args[n], XtNforeground, &fg); n++;
		XtGetValues(text, args, n);
		sprintf (valueString, "%d", fg);
		status = SgtPostSimpleFOREGROUND(top,currentSelection,1,&fg);
		break;
	    }

	    /*
	     * We use the length of the text in the text widget as the value
	     * of the LENGTH target
	     */
	    case LENGTH_DATA: {
		static int length;
		ss = "(number of characters in top text widget)";
		length = strlen(s);
		sprintf (valueString, "%d", length);
		status = SgtPostSimpleLENGTH(top,currentSelection,1,&length);
		break;
	    }

	    case LINE_NUMBER_DATA:
	    {	long *temp = (long *) XtMalloc (2 * sizeof(long));
		ss = "(selected characters in top text widget)";
		temp[0] = left;
		temp[1] = right;
		ss = "(selected characters in top text widget)";
		sprintf (valueString, "%d, %d", temp[0], temp[1]);
		status = SgtPostSimpleLINE_NUMBER(top,currentSelection,2,temp);
		break;
	    }

	    /*
	     * We use the number of lines of text in the text widget as the
	     * value of the LIST_LENGTH target
	     */
	    case LIST_LENGTH_DATA:
	    {	static int lineCount=0;
	   	int i;
		char *cp;
		ss = "(number of lines in top text widget)";
	    	lineCount=0; cp=s;
		if (strlen(s)) lineCount++;
		for (i=1; i<strlen(s); i++) {
			if (*cp++ == '\n') lineCount++;
		}
		sprintf (valueString, "%d", lineCount);
		status = SgtPostSimpleLIST_LENGTH(top,currentSelection,
				1,&lineCount);
		break;
	    }

	    case MODULE_DATA:
	    {	static char *s = "FakeModuleName";
		sprintf (valueString, "%s", s);
		status = SgtPostSimpleMODULE(top,currentSelection, strlen(s),s);
		break;
	    }

	    case MULTIPLE_DATA:
		XmTextSetString (statusMsgW,
		   "IGNORED: handled automatically by libXt");
		XmTextSetString (statusValW, "");
		return;

	    case ODIF_DATA:
	    {	static char *s = "FakeOdifData";
		sprintf (valueString, "%s", s);
		status = SgtPostSimpleODIF(top,currentSelection, strlen(s), s);
		break;
	    }

	    case PROCEDURE_DATA:
	    {	static char *s = "FakeProcedureName";
		sprintf (valueString, "%s", s);
		status = SgtPostSimplePROCEDURE(top,currentSelection,
				strlen(s),s);
		break;
	    }

	    case PROCESS_INTEGER_DATA:
	    {	static pid_t pid;
		pid = getpid();
		sprintf (valueString, "%d", pid);
		status = SgtPostSimplePROCESS_INTEGER(top,currentSelection,
				1, &pid);
		break;
	    }

	    case PROCESS_STRING_DATA:
	    {	pid_t pid = getpid();
		static char buf[10];
		sprintf (buf, "%d\0", pid);
		sprintf (valueString, "%s", buf);
		status = SgtPostSimplePROCESS_STRING(top, currentSelection,
				strlen(buf), buf);
		break;
	    }

	    case STRING_DATA:
		sprintf (valueString, "(top text window)");
		status = SgtPostSimpleSTRING(top,currentSelection,strlen(s),s);
		break;

	    /* Since we don't have a task ID, we fake it with a process ID */
	    case TASK_INTEGER_DATA:
	    {	static pid_t pid;
		ss = "(using the process ID)";
		pid = getpid();
		sprintf (valueString, "%d", pid);
		status = SgtPostSimpleTASK_INTEGER(top,currentSelection,1,&pid);
		break;
	    }

	    /* Since we don't have a task ID, we fake it with a process ID */
	    case TASK_STRING_DATA:
	    {	pid_t pid = getpid();
		static char buf[10];
		ss = "(using the process ID)";
		sprintf (buf, "%d\0", pid);
		sprintf (valueString, "%s", buf);
		status = SgtPostSimpleTASK_STRING(top,currentSelection,
				strlen(buf), buf);
		break;
	    }

	    case TEXT_DATA:
		sprintf (valueString, "(top text window)");
		status = SgtPostSimpleTEXT(top,currentSelection,strlen(s),s);
		break;

	    /* Standard Xmu targets are handled automatically by the library */
	    case TARGETS_DATA:
	    case CLASS_DATA:
	    case CLIENT_WINDOW_DATA:
	    case HOSTNAME_DATA:
	    case IP_ADDRESS_DATA:
	    case NAME_DATA:
	    case OWNER_OS_DATA:
	    case TIMESTAMP_DATA:
	    case USER_DATA:
		XmTextSetString (statusMsgW,
"IGNORED: if something else is posted, this is handled automatically");
		return;

	    case 199 /*no such thing yet*/:
		XmTextSetString (statusMsgW,
"IGNORED: There is no simple select routine for this data type");
		return;


	    /* Other targets are not yet implemented */
	    case BITMAP_DATA:
	    case PIXMAP_DATA:
	    default:
		XmTextSetString (statusMsgW,
"IGNORED: The demo does not know how to select this data type");
		return;
	}

    /* If data was successfully posted, indicate that we have the selection */
	if (status) {
	    char buf[80];
	    sprintf (buf, "Post/select/cut operation succeeded %s", ss);
	    XmTextSetString (statusMsgW, buf);
	    XmTextSetString (statusValW, valueString);
	    if (!isSelected) { /* Add callback for edit -- need to unselect */
		XtAddCallback(text, XmNvalueChangedCallback,textValueCB,NULL);
		indicatePosted();
	    }
	} else {
	    unSelectCB();
	    XmTextSetString (statusMsgW, "Post/select/cut operation failed");
	    XmTextSetString (statusValW, "");
	};
}

/*
 * setSelectionCB() is called when the user activates one of the "PRIMARY"
 * "SECONDARY", CLIPBOARD" or "NONSTANDARD" pushbuttons.  If this changes
 * the selection we are using to transport data, relinqish whatever we had
 * before and post the latest information.
 *
 * NOTE: the second argument ( (XtPointer)client_data ) is actually an Atom.
 */
static void
setSelectionCB(Widget w, Atom cd, XmToggleButtonCallbackStruct *cb) {
	if (!cb->set) return;
	if (currentSelection == cd) return;
	if (isSelected) unPost();
	currentSelection = cd;
	if (isSelected)
		postCB();
}

/*
 * setTargetCB() is called when the user activates one of the various data
 * type pushbuttons (e.g. STRING, TEXT).  If this changes the data type we
 * are transporting, then relinqish whatever we had before and post the
 * latest information.
 */

static void
setTargetCB(Widget w, int cd, XmToggleButtonCallbackStruct *cb) {
	if (!cb->set) return;
	currentTarget = cd;
	if (isSelected)
		postCB();
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
 * debugToggleCB() is called when the "Debug" toggle button changes state.
 * We respond by setting the convenience functions' debug printout to the
 * proper state.
 */
static void
debugToggleCB(Widget w, XtPointer cd, XmToggleButtonCallbackStruct *cb) {
	Arg args[25];
	int n = 0;

	if (cb->set) {
    		SgtSetSimpleSelectDebug(-1);
	} else {
    		SgtSetSimpleSelectDebug(0);
	}
}

/*
 * debugLibToggleCB() is called when the "Debug Library" toggle button changes
 * state.  We respond by setting the convenience functions' debug printout
 * to the proper state.
 */
static void
debugLibToggleCB(Widget w, XtPointer cd, XmToggleButtonCallbackStruct *cb) {
	Arg args[25];
	int n = 0;

	if (cb->set) {
    		SgtSetSimpleSelectLibDebug(-1);
	} else {
    		SgtSetSimpleSelectLibDebug(0);
	}
}

/*
 * insertTextToggleCB() is called when the "Insert Text" toggle button changes
 * proper state.  This controls whether we insert or replace text when we
 * paste.
 */
static void
insertTextToggleCB(Widget w, XtPointer cd, XmToggleButtonCallbackStruct *cb) {
	Arg args[25];
	int n = 0;

	if (cb->set) {
    		insertText = TRUE;
	} else {
    		insertText = FALSE;
	}
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


buildTheGUI () {
    Arg args[25];
    Widget children[256];
    Widget children2[256];
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

    /* RowColumn to hold the radio boxes.  This is needed to get them aligned.*/
    n = 0;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET);    n++;
    XtSetArg(args[n], XmNbottomWidget,     statusMsgW);            n++;
    XtSetArg(args[n], XmNbottomOffset,     2*STD_OFFSET);       n++;
    XtSetArg(args[n], XmNleftAttachment,   XmATTACH_FORM);      n++;
    XtSetArg(args[n], XmNleftOffset,       STD_OFFSET);         n++;
    XtSetArg(args[n], XmNrightAttachment,  XmATTACH_FORM);      n++;
    XtSetArg(args[n], XmNrightOffset,      STD_OFFSET);         n++;
    XtSetArg(args[n], XmNorientation,      XmHORIZONTAL);       n++;
    children[c++] = bigRC = XmCreateRowColumn(form, "bigRC", args, n);

    /* Radio box to choose the selections */
    n = 0;
    selFrame = XmCreateFrame(bigRC, "selectionFrame", args, n);
    selRC = XmCreateRadioBox(selFrame, "selection", args, n);

    c2=0; n=0;
    XtSetArg(args[n], XmNset,              TRUE);               n++;
    children2[c2++] = primaryPB =
	XmCreateToggleButton(selRC, "PRIMARY", args, n);
	XtAddCallback(primaryPB, XmNvalueChangedCallback,
		(XtCallbackProc)setSelectionCB, (XtPointer)XA_PRIMARY);
    n = 0;
    children2[c2++] = secondaryPB =
	XmCreateToggleButton(selRC, "SECONDARY", args, n);
	XtAddCallback(secondaryPB, XmNvalueChangedCallback,
		(XtCallbackProc)setSelectionCB, (XtPointer)XA_SECONDARY);
    n = 0;
    children2[c2++] = clipboardPB =
	XmCreateToggleButton(selRC, "CLIPBOARD", args, n);
	XtAddCallback(clipboardPB, XmNvalueChangedCallback,
		(XtCallbackProc)setSelectionCB, (XtPointer)CLIPBOARD_ATOM);
    n = 0;
    children2[c2++] = nonstandardPB =
	XmCreateToggleButton(selRC, "NONSTANDARD", args, n);
	XtAddCallback(nonstandardPB, XmNvalueChangedCallback,
		(XtCallbackProc)setSelectionCB, (XtPointer)NONSTANDARD_ATOM);
    XtManageChildren(children2, c2);
    XtManageChild(selRC);
    XtManageChild(selFrame);

    /* Radio box to choose the data type */
    n = 0;
    XtSetArg(args[n], XmNorientation,      XmHORIZONTAL);       n++;
    XtSetArg(args[n], XmNnumColumns,       RC_ROWS);            n++;
    XtSetArg(args[n], XmNadjustLast,       FALSE);              n++;
    typeFrame = XmCreateFrame(bigRC, "typeFrame", args, n);
    typeRC = XmCreateRadioBox(typeFrame, "type", args, n);

    c2=0;
    n=0;
    children2[c2++] = backgroundPB =
	XmCreateToggleButton(typeRC, "BACKGROUND", args, n);
	XtAddCallback(backgroundPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) BACKGROUND_DATA);
    n=0;
    children2[c2++] = bitmapPB =
	XmCreateToggleButton(typeRC, "BITMAP", args, n);
	XtAddCallback(bitmapPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) BITMAP_DATA);
    n=0;
    children2[c2++] = character_positionPB =
	XmCreateToggleButton(typeRC, "CHARACTER_POSITION", args, n);
	XtAddCallback(character_positionPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB,
		(XtPointer) CHARACTER_POSITION_DATA);
    n=0;
    children2[c2++] = classPB =
	XmCreateToggleButton(typeRC, "CLASS", args, n);
	XtAddCallback(classPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) CLASS_DATA);
    n=0;
    children2[c2++] = client_windowPB =
	XmCreateToggleButton(typeRC, "CLIENT_WINDOW", args, n);
	XtAddCallback(client_windowPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) CLIENT_WINDOW_DATA);
    n=0;
    children2[c2++] = colormapPB =
	XmCreateToggleButton(typeRC, "COLORMAP", args, n);
	XtAddCallback(colormapPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) COLORMAP_DATA);
    n=0;
    children2[c2++] = column_numberPB =
	XmCreateToggleButton(typeRC, "COLUMN_NUMBER", args, n);
	XtAddCallback(column_numberPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) COLUMN_NUMBER_DATA);
    n=0;
    children2[c2++] = compound_textPB =
	XmCreateToggleButton(typeRC, "COMPOUND_TEXT", args, n);
	XtAddCallback(compound_textPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) COMPOUND_TEXT_DATA);
    n=0;
    children2[c2++] = drawablePB =
	XmCreateToggleButton(typeRC, "DRAWABLE", args, n);
	XtAddCallback(drawablePB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) DRAWABLE_DATA);
    n=0;
    children2[c2++] = deletePB =
	XmCreateToggleButton(typeRC, "DELETE", args, n);
	XtAddCallback(deletePB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) DELETE_DATA);
    n=0;
    children2[c2++] = filenamePB =
	XmCreateToggleButton(typeRC, "FILE_NAME", args, n);
	XtAddCallback(filenamePB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) FILE_NAME_DATA);
    n=0;
    children2[c2++] = foregroundPB =
	XmCreateToggleButton(typeRC, "FOREGROUND", args, n);
	XtAddCallback(foregroundPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) FOREGROUND_DATA);
    n=0;
    children2[c2++] = hostnamePB =
	XmCreateToggleButton(typeRC, "HOSTNAME", args, n);
	XtAddCallback(hostnamePB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) HOSTNAME_DATA);
    n=0;
    children2[c2++] = ip_addressPB =
	XmCreateToggleButton(typeRC, "IP_ADDRESS", args, n);
	XtAddCallback(ip_addressPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) IP_ADDRESS_DATA);
    n=0;
    children2[c2++] = lengthPB =
	XmCreateToggleButton(typeRC, "LENGTH", args, n);
	XtAddCallback(lengthPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) LENGTH_DATA);
    n=0;
    children2[c2++] = line_numberPB =
	XmCreateToggleButton(typeRC, "LINE_NUMBER", args, n);
	XtAddCallback(line_numberPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) LINE_NUMBER_DATA);
    n=0;
    children2[c2++] = list_lengthPB =
	XmCreateToggleButton(typeRC, "LIST_LENGTH", args, n);
	XtAddCallback(list_lengthPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) LIST_LENGTH_DATA);
    n=0;
    children2[c2++] = modulePB =
	XmCreateToggleButton(typeRC, "MODULE", args, n);
	XtAddCallback(modulePB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) MODULE_DATA);
    n=0;
    children2[c2++] = multiplePB =
	XmCreateToggleButton(typeRC, "MULTIPLE", args, n);
	XtAddCallback(multiplePB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) MULTIPLE_DATA);
    n=0;
    children2[c2++] = namePB =
	XmCreateToggleButton(typeRC, "NAME", args, n);
	XtAddCallback(namePB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) NAME_DATA);
    n=0;
    children2[c2++] = odifPB =
	XmCreateToggleButton(typeRC, "ODIF", args, n);
	XtAddCallback(odifPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) ODIF_DATA);
    n=0;
    children2[c2++] = owner_osPB =
	XmCreateToggleButton(typeRC, "OWNER_OS", args, n);
	XtAddCallback(owner_osPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) OWNER_OS_DATA);
    n=0;
    children2[c2++] = pixmapPB =
	XmCreateToggleButton(typeRC, "PIXMAP", args, n);
	XtAddCallback(pixmapPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) PIXMAP_DATA);
    n=0;
    children2[c2++] = procedurePB =
	XmCreateToggleButton(typeRC, "PROCEDURE", args, n);
	XtAddCallback(procedurePB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) PROCEDURE_DATA);
    n=0;
    children2[c2++] = processIntegerPB =
	XmCreateToggleButton(typeRC, "PROCESS (integer)", args, n);
	XtAddCallback(processIntegerPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) PROCESS_INTEGER_DATA);
    n=0;
    children2[c2++] = processStringPB =
	XmCreateToggleButton(typeRC, "PROCESS (string)", args, n);
	XtAddCallback(processStringPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) PROCESS_STRING_DATA);
    n=0;
    XtSetArg(args[n], XmNset,              TRUE);               n++;
    children2[c2++] = stringPB =
	XmCreateToggleButton(typeRC, "STRING", args, n);
	XtAddCallback(stringPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) STRING_DATA);
    n = 0;
    children2[c2++] = targetsPB =
	XmCreateToggleButton(typeRC, "TARGETS", args, n);
	XtAddCallback(targetsPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) TARGETS_DATA);
    n = 0;
    children2[c2++] = taskIntegerPB =
	XmCreateToggleButton(typeRC, "TASK (integer)", args, n);
	XtAddCallback(taskIntegerPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) TASK_INTEGER_DATA);
    n = 0;
    children2[c2++] = taskStringPB =
	XmCreateToggleButton(typeRC, "TASK (string)", args, n);
	XtAddCallback(taskStringPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) TASK_STRING_DATA);
    n = 0;
    children2[c2++] = textPB =
	XmCreateToggleButton(typeRC, "TEXT", args, n);
	XtAddCallback(textPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) TEXT_DATA);
    n = 0;
    children2[c2++] = timestampPB =
	XmCreateToggleButton(typeRC, "TIMESTAMP", args, n);
	XtAddCallback(timestampPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) TIMESTAMP_DATA);
    n = 0;
    children2[c2++] = userPB =
	XmCreateToggleButton(typeRC, "USER", args, n);
	XtAddCallback(userPB, XmNvalueChangedCallback,
		(XtCallbackProc)setTargetCB, (XtPointer) USER_DATA);
    XtManageChildren(children2, c2);
    XtManageChild(typeRC);
    XtManageChild(typeFrame);

    /* RowColumn for miscellaneous things */
    n = 0;
    optionFrame = XmCreateFrame(bigRC, "optionFrame", args, n);
    optionRC = XmCreateRowColumn(optionFrame, "options", args, n);

    n=0; c2=0;
    XtSetArg(args[n], XmNalignment,        XmALIGNMENT_BEGINNING); n++;
    children2[c2++] = debugTogglePB =
	XmCreateToggleButton(optionRC, "Debug", args, n);
	XtAddCallback(debugTogglePB, XmNvalueChangedCallback,
		(XtCallbackProc)debugToggleCB, NULL);
    n=0;
    children2[c2++] = debugLibTogglePB =
	XmCreateToggleButton(optionRC, "Debug Library", args, n);
	XtAddCallback(debugTogglePB, XmNvalueChangedCallback,
		(XtCallbackProc)debugLibToggleCB, NULL);
    n=0;
    children2[c2++] = insertTextTogglePB =
	XmCreateToggleButton(optionRC, "Insert Text", args, n);
	XtAddCallback(insertTextTogglePB, XmNvalueChangedCallback,
		(XtCallbackProc)insertTextToggleCB, NULL);
    XtManageChildren(children2, c2);
    XtManageChild(optionRC);
    XtManageChild(optionFrame);

    /* Text widget to cut and paste text to/from */
    n=0;
    XtSetArg(args[n], XmNtopAttachment,    XmATTACH_FORM);     n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET);   n++;
    XtSetArg(args[n], XmNbottomWidget,     bigRC);             n++;
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
