/*
 * This source conditionally compiles into either an IrisGL or an OpenGL
 * color chooser widget demo:
 *
 *	*) by default, the IrisGL color chooser demo is produced
 *
 *	*)if "OGL" is defined, the openGL color chooser demo is produced
 */

#include <stdlib.h>
#include <stdio.h>
#include <X11/Intrinsic.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/MainW.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>

#include <Sgm/ColorC.h>

/*
 * These definitions are all that controls the difference between an
 * IrisGl demo and an OpenGL demo.
 * 
 * Other differences are:
 *	* SgIsColorChooser, SgIglIsColorChooser, and SgOglIsColorChooser,
 *	  which are all used below.
 *
 *	* sgColorChooserWidgetClass vs. sgOglColorChooserWidgetClass, which
 *	  are not explicitly used in this demo.
 */
#ifdef OGL /* must be building the IrisGL demo */
#  define SgCreateColorChooser SgOglCreateColorChooser
#  define SgCreateColorChooserDialog SgOglCreateColorChooserDialog
#  define NAME "ogl_test"
#else      /* must be building the IrisGL demo */
#  define NAME "igl_test"
#endif     /* OGL */


/* Function prototypes */
	static void fileCB(Widget, XtPointer, XtPointer);
	static void helpCB(Widget, XtPointer, XtPointer);
	static void okDialogCB ( Widget, XtPointer, XtPointer );
	static void applyDialogCB ( Widget, XtPointer, XtPointer );
	static void cancelDialogCB ( Widget, XtPointer, XtPointer );
	static void dragDialogCB ( Widget, XtPointer, XtPointer );
	static void helpDialogCB ( Widget, XtPointer, XtPointer );
	static void valueChangedDialogCB ( Widget, XtPointer, XtPointer );
	static void okMainCB ( Widget, XtPointer, XtPointer );
	static void applyMainCB ( Widget, XtPointer, XtPointer );
	static void cancelMainCB ( Widget, XtPointer, XtPointer );
	static void dragMainCB ( Widget, XtPointer, XtPointer );
	static void helpMainCB ( Widget, XtPointer, XtPointer );
	static void valueChangedMainCB ( Widget, XtPointer, XtPointer );


/* Static variables */
	Widget topLevel;
	Widget reasonLW, reasonTW;
	Widget mainCC, dialogCC;
	Widget rTW, gTW, bTW, hTW, sTW, vTW;
	int rgbBase;


/* Fallback resources */
	String fallbackResources[] = {
		"*allowShellResize: True",
		"      *useSchemes: all",
		"          *scheme: Base",
		NULL
	};



main( int argc, char *argv[]) {
    Widget mainW, menuB, workA;
    XtAppContext app;

    /* Initialize Xt, parse standard Xt command line options */
	topLevel = XtVaAppInitialize (&app, "Demo",
		(XrmOptionDescList)NULL, 0, &argc, argv,
		fallbackResources, NULL);

    /* Create the widgets */
	mainW = XtVaCreateManagedWidget ("main",
		xmMainWindowWidgetClass,   topLevel,
		XmNvisualPolicy, XmVARIABLE,
		NULL);

	/* Create the menubar */
	{   XmString file = XmStringCreateLocalized ("File");
	    XmString help = XmStringCreateLocalized ("Help");

	    menuB = XmVaCreateSimpleMenuBar (mainW, "menubar",
		XmVaCASCADEBUTTON, file, 'F',
		XmVaCASCADEBUTTON, help, 'H',
		NULL);
	    XmStringFree(file);
	    XmStringFree(help);

	    /* Set the Help item */
	    XtVaSetValues (menuB, XmNmenuHelpWidget,
		XtNameToWidget(menuB, "button_1"), NULL);
	}

	/* Create the "File" pulldown menu */
	{   XmString base       = XmStringCreateLocalized ("Hex <-> Decimal");
	    XmString dialog     = XmStringCreateLocalized ("Dialog");
	    XmString dialogAcc  = XmStringCreateLocalized ("Alt-D");
	    XmString quit       = XmStringCreateLocalized ("Quit");
	    XmString quitAcc    = XmStringCreateLocalized ("Ctrl-C");
	    Widget menuF;

	    menuF = XmVaCreateSimplePulldownMenu(menuB, "file_menu", 0, fileCB,
		XmVaPUSHBUTTON, base, 'B', NULL, NULL,
		XmVaPUSHBUTTON, dialog, 'D', "Alt<Key>D", dialogAcc,
		XmVaSEPARATOR,
		XmVaPUSHBUTTON, quit, 'Q', "Ctrl<Key>C", quitAcc,
		NULL);
	    XmStringFree (base);
	    XmStringFree (dialog);
	    XmStringFree (dialogAcc);
	    XmStringFree (quit);
	    XmStringFree (quitAcc);
	}

	/* Create the "Help" pulldown menu */
	{   XmString help = XmStringCreateLocalized ("Help on this demo");
	    Widget menuH;
	    
	    menuH = XmVaCreateSimplePulldownMenu(menuB, "help_menu", 1, helpCB,
		XmVaPUSHBUTTON, help, 'H', NULL, NULL,
		NULL);
	    XmStringFree(help);
	}

	/*
	 * Create the workarea -- a color chooser.
	 *
	 * Also add the status stuff to the workarea, because
	 * of problems getting it to work in the message area.
	 */
	{   Widget sep1, sLW;

	    workA = XtVaCreateManagedWidget("form",
		xmFormWidgetClass, mainW, NULL);
	    mainCC = SgCreateColorChooser( workA, "chooser", NULL, 0);
	    if (SgIsColorChooser(mainCC))
fprintf (stderr, "The widget is a color chooser widget\n");
	    else
fprintf (stderr, "The widget is not a color chooser widget\n");
	    if (SgIglIsColorChooser(mainCC))
fprintf (stderr, "The color chooser widget is an IrisGL color chooser\n");
	    else
fprintf (stderr, "The color chooser widget is not an IrisGL color chooser\n");
	    if (SgOglIsColorChooser(mainCC))
fprintf (stderr, "The color chooser widget is an OpenGL color chooser\n");
	    else
fprintf (stderr, "The color chooser widget is not an OpenGL color chooser\n");
	    XtVaGetValues( mainCC, SgNrgbBase, &rgbBase, NULL); /* User may have set it */
	    SgColorChooserSetColor(mainCC, 62, 24, 111);
	    XtVaSetValues( mainCC,
		XmNtopAttachment,    XmATTACH_FORM,
		XmNleftAttachment,   XmATTACH_FORM,
		XmNrightAttachment,  XmATTACH_FORM,
		NULL);
	    XtManageChild(mainCC);
	    XtAddCallback(mainCC, XmNokCallback, okMainCB, NULL);
	    XtAddCallback(mainCC, XmNapplyCallback, applyMainCB, NULL);
	    XtAddCallback(mainCC, XmNcancelCallback, cancelMainCB, NULL);
	    XtAddCallback(mainCC, XmNdragCallback, dragMainCB, NULL);
	    XtAddCallback(mainCC, XmNhelpCallback, helpMainCB, NULL);
	    XtAddCallback(mainCC, XmNvalueChangedCallback, valueChangedMainCB,NULL);

	    /* Create a separator to denote the status area */
	    sep1 = XtVaCreateManagedWidget( "sep1",
		xmSeparatorWidgetClass, workA,
		XmNshadowThickness,  10,
		XmNheight,           30,
		XmNtopAttachment,    XmATTACH_WIDGET,
		XmNtopWidget,        mainCC,
		XmNleftAttachment,   XmATTACH_FORM,
		XmNrightAttachment,  XmATTACH_FORM,
		NULL);

	    /* Create a label to annonce the status area */
	    {	XmString sLabel = XmStringCreateLocalized(
		 "Application (not widget) Maintains The Following Status Area: ");
		sLW = XtVaCreateManagedWidget ("sLabel",
			xmLabelWidgetClass,  workA,
			XmNlabelString,      sLabel,
			XmNmarginBottom,     10,
			XmNtopAttachment,    XmATTACH_WIDGET,
			XmNtopWidget,        sep1,
			XmNleftAttachment,   XmATTACH_FORM,
			XmNrightAttachment,  XmATTACH_FORM,
			NULL);
		XmStringFree(sLabel);
	    }

	    /* Set up the "Last Callback:" line */
	    {	XmString reasonLabel =
			XmStringCreateLocalized ("Last Callback: ");
		Dimension height;

		reasonLW = XtVaCreateManagedWidget ("reasonLabel",
			xmLabelWidgetClass,  workA,
			XmNlabelString,      reasonLabel,
			XmNtopAttachment,    XmATTACH_WIDGET,
			XmNtopWidget,        sLW,
			XmNleftAttachment,   XmATTACH_FORM,
			NULL);
		reasonTW = XtVaCreateManagedWidget ("reasonText",
			xmTextWidgetClass,   workA,
			XmNeditable,         False,
			XmNcolumns,          30,
			XmNtopAttachment,    XmATTACH_WIDGET,
			XmNtopWidget,        sLW,
			XmNleftAttachment,   XmATTACH_WIDGET,
			XmNleftWidget,       reasonLW,
			XmNrightAttachment,  XmATTACH_FORM,
			NULL);
		XtVaGetValues( reasonTW, XmNheight, &height, NULL);
		XtVaSetValues( reasonLW, XmNheight,  height, NULL);
		XmStringFree(reasonLabel);
	    }

	    /* Set up the RBGHSV report areas to log changes as they happen*/
	    {	Widget rLW, gLW, bLW, hLW, sLW, vLW;
	    	Widget rc;

	    	XmString rLabel = XmStringCreateLocalized ("Red: ");
	    	XmString gLabel = XmStringCreateLocalized ("Green: ");
	    	XmString bLabel = XmStringCreateLocalized ("Blue: ");

		rc = XtVaCreateManagedWidget("rc",
			xmRowColumnWidgetClass, workA,
			XmNnumColumns,       3,
			XmNpacking,          XmPACK_COLUMN,
			XmNtopAttachment,    XmATTACH_WIDGET,
			XmNtopWidget,        reasonTW,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment,   XmATTACH_FORM,
			XmNrightAttachment,  XmATTACH_FORM,
			NULL);
		rLW = XtVaCreateManagedWidget ("rLabel",
			xmLabelWidgetClass,  rc,
			XmNlabelString,  rLabel,
			NULL);
		rTW = XtVaCreateManagedWidget ("rText",
			xmTextWidgetClass,   rc,
			XmNeditable,     False,
			/* DEBUG XmNcolumns,      15, */
			NULL);
		gLW = XtVaCreateManagedWidget ("gLabel",
			xmLabelWidgetClass,  rc,
			XmNlabelString,  gLabel,
			NULL);
		gTW = XtVaCreateManagedWidget ("gText",
			xmTextWidgetClass,   rc,
			XmNeditable,     False,
			/* DEBUG XmNcolumns,      10, */
			NULL);
		bLW = XtVaCreateManagedWidget ("bLabel",
			xmLabelWidgetClass,  rc,
			XmNlabelString,  bLabel,
			NULL);
		bTW = XtVaCreateManagedWidget ("bText",
			xmTextWidgetClass,   rc,
			XmNeditable,     False,
			/* DEBUG XmNcolumns,      15, */
			NULL);
	
	    	XmStringFree(rLabel);
	    	XmStringFree(gLabel);
	    	XmStringFree(bLabel);
	    }
	}


    /* Start it all going */
	XtManageChild(menuB);
	XtManageChild(workA);
	XmMainWindowSetAreas (mainW, menuB, NULL, NULL, NULL, workA);
	XtRealizeWidget (topLevel);
	XtAppMainLoop(app);
}


static void fileCB(Widget w, XtPointer client_data, XtPointer call_data) {
	switch ((int)(client_data)) {

	case 0:	rgbBase = rgbBase==10 ? 16 : 10;
		XtVaSetValues (mainCC, SgNrgbBase, rgbBase, NULL);
		if (dialogCC) 
			XtVaSetValues (dialogCC, SgNrgbBase, rgbBase, NULL);
		break;

	case 1:	if (!dialogCC) {
			Dimension width=0, height=0;
			Position x=0, y=0;
			dialogCC = SgCreateColorChooserDialog(
				topLevel, "popup", NULL, 0);
			XtVaSetValues (dialogCC, SgNrgbBase, rgbBase, NULL);
			SgColorChooserSetCurrentColor(dialogCC, 128, 0, 0);
			SgColorChooserSetStoredColor(dialogCC, 0, 128, 0);
			XtAddCallback(dialogCC, XmNokCallback,
					okDialogCB, NULL);
			XtAddCallback(dialogCC, XmNapplyCallback,
					applyDialogCB, NULL);
			XtAddCallback(dialogCC, XmNcancelCallback,
					cancelDialogCB, NULL);
			XtAddCallback(dialogCC, XmNdragCallback,
					dragDialogCB, NULL);
			XtAddCallback(dialogCC, XmNhelpCallback,
					helpDialogCB, NULL);
			XtAddCallback(dialogCC, XmNvalueChangedCallback,
					valueChangedDialogCB, NULL);
			/* Initial position of popup to not obscure parent */
			XtVaGetValues(topLevel, XmNwidth, &width,
						XmNheight, &height,
						XmNx, &x, XmNy, &y,
						NULL);
			XtVaSetValues(dialogCC, XmNdefaultPosition, False,
					  XmNx, x+width,
					  XmNy, y+height,
					  NULL);
		}
		XtManageChild(dialogCC);
		break;

	case 2:
		exit(0);

	default:
		fprintf (stderr, "ERROR: Got unknown item %d\n", client_data);
	}
}


/*
 * Set the status fields for callbacks from buttons.
 * In this case, we still need to fetch the values from the widget.
*/
static void buttonStatus(Widget w, char *s) {
    short r, g, b;
    char c[256];
	XmTextSetString( reasonTW, s);
	SgColorChooserGetColor (w, &r, &g, &b);
	sprintf (c, "%d\0", r); XmTextSetString( rTW, c);
	sprintf (c, "%d\0", g); XmTextSetString( gTW, c);
	sprintf (c, "%d\0", b); XmTextSetString( bTW, c);
}


/*
 * Set the status fields for value change callbacks (drag, and value_changed).
 * These happen when the user interacts directly with the widget.
 * In this case, the values are passed as part of the callback structure.
 */
static void valueStatus(Widget w,
    SgColorChooserCallbackStruct *call_data, char *s) {
    char c[256];
	XmTextSetString( reasonTW, s);
	sprintf (c, "%d\0", call_data->r); XmTextSetString( rTW, c);
	sprintf (c, "%d\0", call_data->g); XmTextSetString( gTW, c);
	sprintf (c, "%d\0", call_data->b); XmTextSetString( bTW, c);
}


static void helpCB ( Widget w, XtPointer client_data, XtPointer call_data) {
	fprintf (stderr, "Help is not implemented in this demo\n");
}

/* Callbacks from the dialog window color chooser */

static void okDialogCB (
	Widget w, XtPointer client_data, XtPointer call_data) {
	buttonStatus(w,"OK  (Dialog)");
}

static void applyDialogCB (
	Widget w, XtPointer client_data, XtPointer call_data) {
	buttonStatus(w,"APPLY  (Dialog)");
}

static void cancelDialogCB (
	Widget w, XtPointer client_data, XtPointer call_data) {
	buttonStatus(w,"CANCEL  (Dialog)");
}

static void dragDialogCB (
	Widget w, XtPointer client_data, XtPointer call_data) {
	valueStatus(w, (SgColorChooserCallbackStruct*)call_data, "DRAG  (Dialog)");
}

static void helpDialogCB (
	Widget w, XtPointer client_data, XtPointer call_data) {
	buttonStatus(w,"HELP  (Dialog)");
}

static void valueChangedDialogCB (
	Widget w, XtPointer client_data, XtPointer call_data) {
	valueStatus(w, (SgColorChooserCallbackStruct*)call_data, "VALUE CHANGED  (Dialog)");
}

/* Callbacks from the main window color chooser */
static void okMainCB (
	Widget w, XtPointer client_data, XtPointer call_data) {
	buttonStatus(w,"OK  (Main)");
}

static void applyMainCB (
	Widget w, XtPointer client_data, XtPointer call_data) {
	buttonStatus(w,"APPLY  (Main)");
}

static void cancelMainCB (
	Widget w, XtPointer client_data, XtPointer call_data) {
	buttonStatus(w,"CANCEL  (Main)");
}

static void dragMainCB (
	Widget w, XtPointer client_data, XtPointer call_data) {
	valueStatus(w, (SgColorChooserCallbackStruct*)call_data, "DRAG  (Main)");
}

static void helpMainCB (
	Widget w, XtPointer client_data, XtPointer call_data) {
	buttonStatus(w,"HELP  (Main)");
}

static void valueChangedMainCB (
	Widget w, XtPointer client_data, XtPointer call_data) {
	valueStatus(w, (SgColorChooserCallbackStruct*)call_data, "VALUE CHANGED  (Main)");
}

#ifdef NEVER
##### This is never used.
##### I am not sure that it should not be hooked up and used, though,
##### so I am leaving it in.
##### It can be removed by the first person who is sure it is useless.
static void ColorChangeCallback(Widget w, void *client_data,
                                SgColorChooserCallbackStruct *call_data)
{
  Boolean *drag = (Boolean *) client_data;
  char    color_text[128];

  if(call_data->reason == XmCR_DRAG)
    sprintf(color_text, "Drag color values: R %d, G %d, B %d", call_data->r,
            call_data->g, call_data->b);
  else
    sprintf(color_text, "ChangedValue color values: R %d, G %d, B %d",
            call_data->r, call_data->g, call_data->b);

  XmTextSetString(text_widget, color_text);
}

#endif
