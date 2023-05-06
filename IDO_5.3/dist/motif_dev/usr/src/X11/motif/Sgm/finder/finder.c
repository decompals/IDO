/*
 * Finder.c demonstrates the use of the SgFinder widget
 */
#include <stdlib.h>
#include <stdio.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Sgm/Finder.h>
#include <Sgm/DynaMenu.h>

static char * items[] = { "Archer's favorite songs:",
			    "Draft dodger rag",
			    "Le Roi Renaud",
			    "/usr/sbin",
			    "/lib/libc.so.1",
			    "Calvinist Headgear Expressway",
			  };

static void valueChangeCB( Widget w, XtPointer clientData, XmAnyCallbackStruct * cbs) {
  printf("App value change callback\n");
}

static void activateCB( Widget w, XtPointer clientData, XmAnyCallbackStruct * cbs) {
  printf("App activate callback\n");
}

main( int argc, char * argv[] ) {
  Widget toplevel, rc, label, finder, history;
  XtAppContext app;
  XmString * list;
  int listSize, i;
  
  XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);
  toplevel = XtVaAppInitialize( &app, "Finder", NULL, 0, &argc, argv, NULL, NULL);
  rc = XtVaCreateWidget( "rc",
			xmRowColumnWidgetClass, toplevel,
			XmNresizeWidth, False,
			XmNresizeHeight, True,
			NULL);
  
  /* create the original list for the historyMenu */
  listSize = XtNumber( items );
  list = (XmString *)XtMalloc( sizeof(XmString) * listSize);
  for (i = 0; i < listSize; i++)
    list[ i ] = XmStringCreateLocalized( items[ i ] );
  
  label = XtVaCreateManagedWidget( "Things:",
				  xmLabelWidgetClass, rc,
				  NULL);
  finder = XtVaCreateManagedWidget("finder",
				   sgFinderWidgetClass, rc,
				   NULL);
  history = SgFinderGetChild( finder, SgFINDER_HISTORY_MENUBAR );
  if (history && SgIsDynaMenu( history )) {
    XtVaSetValues( history,
		  SgNhistoryListItems, list,
		  SgNhistoryListItemCount, listSize,
		  NULL);
  }
  
  for (i = 0; i < listSize; i++)
    if (list[ i ])
      XmStringFree(list[ i ]);
  if (list)
    XtFree( (char *)list );
  
  XtAddCallback( finder, XmNvalueChangedCallback, (XtCallbackProc)valueChangeCB, finder);
  XtAddCallback( finder, XmNactivateCallback, (XtCallbackProc)activateCB, finder);
  
  XtManageChild( rc );
  XtRealizeWidget( toplevel );
  XtAppMainLoop( app );
}



