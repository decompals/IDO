#include <stdio.h>
#include <Sgm/DynaMenu.h>
#include <Xm/RowColumn.h>

static char * items[] = { "illegal smile", "/usr/people/stone",
			  "Fish and whistle", "help I'm trapped in the machine",
			  "9th & Hennepin" };

static void dynaPushCB( Widget w, XtPointer clientData, XtPointer cbd ) {
  SgDynaMenuCallbackStruct * cbs = (SgDynaMenuCallbackStruct *) cbd;
  XtArgVal num = cbs->button_number;
  printf("Selected item number %d\n", num);
}

void
main( int argc, char * argv[] ) {
  XtAppContext app = NULL;
  Widget toplevel, rc, dynaMenu;
  XmString * list;
  int listSize, i;
  
  toplevel = XtVaAppInitialize( &app, "DynaMenu", NULL, 0, &argc, argv, NULL, NULL);
  rc = XtVaCreateManagedWidget( "rc", xmRowColumnWidgetClass, toplevel, NULL);

  /* create the original list for the dynaMenu */
  listSize = XtNumber( items );
  list = (XmString *)XtMalloc( sizeof(XmString) * (unsigned int)listSize);
  for (i = 0; i < listSize; i++)
    list[ i ] = XmStringCreateLocalized( items[ i ] );
  
  dynaMenu = XtVaCreateManagedWidget("dynaMenu",
				     sgDynaMenuWidgetClass, rc,
				     SgNhistoryListItems, list,
				     SgNhistoryListItemCount, listSize,
				     NULL);

  XtAddCallback( dynaMenu, SgNdynaPushCallback, dynaPushCB, NULL);
  
  for (i = 0; i < listSize; i++)
    XmStringFree( list[ i ] );
  XtFree( (char *)list );
  
  XtRealizeWidget( toplevel );
  XtAppMainLoop( app );
}

