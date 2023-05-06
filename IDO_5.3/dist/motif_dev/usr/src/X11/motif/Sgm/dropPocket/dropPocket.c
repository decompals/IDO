/*
 *  Demonstrate the use of the DropPocket
 */

#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Sgm/DropPocket.h>

static void droppedCB(Widget w, XtPointer clientData, XtPointer cbs ) {
  SgDropPocketCallbackStruct * dcbs = (SgDropPocketCallbackStruct *)cbs;
  char * name;

  if (dcbs->iconName)
    if (!XmStringGetLtoR( dcbs->iconName, XmFONTLIST_DEFAULT_TAG, &name))
      name = NULL;
  
  printf("Dropped file: %s\nFull Data: %s\n", name, dcbs->iconData );
  XtFree( name );
}

main( int argc, char * argv[] ) {
  Widget toplevel, exitB, dp, topRC;
  XtAppContext app;

  XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);
  toplevel = XtVaAppInitialize( &app, "DropPocket", NULL, 0, &argc, argv, NULL, NULL);
  topRC = XtVaCreateManagedWidget( "topRC",
				  xmFormWidgetClass, toplevel,
				  NULL);
  dp = XtVaCreateManagedWidget("dp",
			       sgDropPocketWidgetClass, topRC,
			       XmNtopAttachment, XmATTACH_FORM,
			       XmNbottomAttachment, XmATTACH_FORM,
			       XmNleftAttachment, XmATTACH_FORM,
			       XmNrightAttachment, XmATTACH_FORM,
			       XmNheight, 100,
			       XmNwidth, 100,
			       NULL);
  XtAddCallback( dp, SgNiconUpdateCallback, droppedCB, NULL);
  XtRealizeWidget( toplevel );
  XtAppMainLoop( app );
}


