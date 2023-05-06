#include <Xm/FileSB.h>
#include <Xm/TextF.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <stdlib.h>
#include <stdio.h>

void printDirF( Widget w, XtPointer clientData, XmFileSelectionBoxCallbackStruct * cbs) {
  char * filename = NULL, * dirname = NULL;
  XmString spec, dir;
  char * dirText, * specText;
  Widget me = (Widget)clientData;
  
  XmStringGetLtoR( cbs->value, XmFONTLIST_DEFAULT_TAG, &filename);
  XmStringGetLtoR( cbs->dir, XmFONTLIST_DEFAULT_TAG, &dirname);
  XtVaGetValues(me,
		XmNdirectory, &dir,
		XmNdirSpec, &spec,
		NULL);
  printf("Filename selected: %s\n", filename);
  printf("Dirname selected: %s\n", dirname);
  XmStringGetLtoR( dir, XmFONTLIST_DEFAULT_TAG, &dirText);
  XmStringGetLtoR( spec, XmFONTLIST_DEFAULT_TAG, &specText);
  
  printf("Directory resource: %s\n", dirText);
  printf("Full selection resource: %s\n", specText);
  
  XtFree( filename );
  XtFree( dirname );
  XtFree( dirText );
  XtFree( specText );
}

static void showDialog( Widget w, XtPointer clientData, XtPointer callData) {
  
  Widget dialog = (Widget) clientData;
  XtManageChild( dialog );
  
}

main (int argc, char *argv[]) {
  Widget toplevel, fsb, b1, b2, form;
  XtAppContext app;
  
  XtSetLanguageProc( NULL, (XtLanguageProc)NULL, NULL);
  
  toplevel = XtVaAppInitialize(&app, "Fsb", NULL, 0, &argc, argv, NULL, NULL);
  
  form = XtVaCreateManagedWidget( "form",
				  xmFormWidgetClass, toplevel,
				  NULL);
  
  if (argc > 1) {
    Arg args[2];
    XmString dir = XmStringCreateLocalized( "/usr" );
    int n = 0;
    XtSetArg( args[ n ], XmNdirectory, dir); n++;
    
    b1 = XtVaCreateManagedWidget( "FSB",
				  xmPushButtonWidgetClass, form,
				  XmNtopAttachment, XmATTACH_FORM,
				  XmNbottomAttachment, XmATTACH_FORM,
				  XmNleftAttachment, XmATTACH_FORM,
				  XmNrightAttachment, XmATTACH_FORM,
				  NULL);
    
    /* Make an XmFileSelectionDialog as a child of the PushButton */
    fsb = XmCreateFileSelectionDialog( b1, "FSB Dialog", args, n);
    XmStringFree( dir );
    XtAddCallback( b1, XmNactivateCallback, showDialog, fsb);
  } else {
    /* Make an XmFileSelectionBox and place it on a form */
    fsb = XmCreateFileSelectionBox( form, "Select A File", NULL, 0);
    XtVaSetValues( fsb,
		   XmNtopAttachment, XmATTACH_FORM,
		   XmNbottomAttachment, XmATTACH_FORM,
		   XmNleftAttachment, XmATTACH_FORM,
		   XmNrightAttachment, XmATTACH_FORM,
		   NULL);
    XtManageChild( fsb );
  }
  
  XtAddCallback( fsb, XmNokCallback, (XtCallbackProc)printDirF, fsb);
  XtAddCallback( fsb, XmNcancelCallback, (XtCallbackProc)exit, NULL);
  
  XtRealizeWidget( toplevel );
  XtAppMainLoop( app );
  
}





