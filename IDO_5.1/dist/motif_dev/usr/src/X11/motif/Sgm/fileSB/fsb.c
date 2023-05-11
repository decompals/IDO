#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>

#include <Xm/FileSB.h>
    
void printDirF( Widget w, XtPointer clientData, XmFileSelectionBoxCallbackStruct * cbs) {
  char * filename = NULL, * dirname = NULL;
  int items;
  XmString spec;
  char * specText;
  XmString dir;
  char * dirText;
  Widget me = (Widget)clientData;

  XmStringGetLtoR( cbs->value, XmFONTLIST_DEFAULT_TAG, &filename);
  XmStringGetLtoR( cbs->dir, XmFONTLIST_DEFAULT_TAG, &dirname);
  XtVaGetValues(me,
		XmNfileListItemCount, &items,
		XmNdirectory, &dir,
		XmNdirSpec, &spec,
		NULL);
  printf("Filename selected: %s\n", filename);
  printf("Dirname selected: %s\n", dirname);
  XmStringGetLtoR( dir, XmFONTLIST_DEFAULT_TAG, &dirText);
  XmStringGetLtoR( spec, XmFONTLIST_DEFAULT_TAG, &specText);

  printf("Filename selected: %s\n", filename);
  printf("Directory resource: %s\n", dirText);
  printf("Full selection resource: %s\n", specText);

  if (filename)
    XtFree( filename );
  if (dirname)
    XtFree( dirname );
  if (dirText)
    XtFree( dirText );
  if (specText)
    XtFree( specText );
}

static void showDialog( Widget w, XtPointer clientData, XtPointer callData) {

  Widget dialog = (Widget) clientData;
  XtManageChild( dialog );
 
}
 
main (int argc, char *argv[]) {
  Widget toplevel, fsb, b1, b2, rc;
  XtAppContext app;
  XmString textStr;

  XtSetLanguageProc( NULL, (XtLanguageProc)NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Fsb", NULL, 0, &argc, argv, NULL, NULL);

  rc = XtVaCreateManagedWidget( "rc",
			       xmFormWidgetClass, toplevel,
			       NULL);

if (argc > 1) {

  Arg args[2];
  XmString dir;
  int n = 0;
  dir = XmStringCreateLocalized( "/usr/people/johnk" );
  XtSetArg( args[ n ], XmNdirectory, dir); n++;

  b1 = XtVaCreateManagedWidget( "FSB",
			       xmPushButtonWidgetClass, rc,
			       XmNtopAttachment, XmATTACH_FORM,
			       XmNbottomAttachment, XmATTACH_FORM,
			       XmNleftAttachment, XmATTACH_FORM,
			       XmNrightAttachment, XmATTACH_FORM,
			       NULL);

			
  fsb = XmCreateFileSelectionDialog( b1, "FSB Dialog", args, n);

    XmStringFree( dir );


  XtAddCallback( b1, XmNactivateCallback, showDialog, fsb);

} else {

  fsb = XmCreateFileSelectionBox( rc, "Select A File", NULL, 0);
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


