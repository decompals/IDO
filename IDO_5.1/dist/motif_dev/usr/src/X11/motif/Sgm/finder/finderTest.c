#include <assert.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <stdlib.h>
#include <stdio.h>
#include <Sgm/Finder.h>
#include <Sgm/DynaMenu.h>

#define tbmap_width 16
#define tbmap_height 16
#define tbmap_x_hot 6
#define tbmap_y_hot 8
static char tbmap_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0xc0, 0x07, 0x60, 0x0c, 0x60, 0x0c,
   0xcc, 0x07, 0x9e, 0x0f, 0xb3, 0x39, 0x21, 0x61, 0x73, 0x02, 0xee, 0x06,
   0xa6, 0x05, 0x20, 0x01, 0x30, 0x00, 0x30, 0x00};


static char * items[] = { "/usr/local",
			  "/usr/people/johnk",
			  "/hosts/bambi",
			  "help I'm trapped in the machine",
			};



static char * newItems[] = { "newItem1",
 "newItem2",
 "newItem3",
 "newItem4",
 "newItem5",
 "newItem6",
 "newItem7",
};



static char * myItems[] = { "Item 1",
 "Item 2",
 "Item 3",
 "Item 4",
 "Item 5",
 "Item 6",
 "Item 7",
 "Item 8",
 "Item 9",
 "Item 10",
 "Item 11",
 "Item 12",
 "Item 13",
 "Item 14",
	};



static void mySetTextSection( SgFinderWidget ft, int section ) {

  char * str = SgFinderGetTextString( ft );
  int i, count = -1;
  char separator;
  
  XtVaGetValues( (Widget)ft,
		SgNseparator, &separator,
		NULL);

  for (i = 0; i < strlen( str ); i++) {
    if (str[ i ] == separator) {
      count++;
      if (count == section) {
	/* special case for 0th button */
	if ( section == 0 ) {
	  /* set the string to the separator */
	  char newStr[2];
	  newStr[0] = separator;
	  newStr[1] = '\0';
	  SgFinderSetTextString( ft, newStr );
	} else {
	  /* set the text to the str up to section */
	  char * textStr = (char *)malloc( sizeof(char) * i + 1);
	  strncpy( textStr, str, i);
	  textStr[ i ] = '\0';
	  SgFinderSetTextString( ft, textStr );
	  if ( textStr )
	    free( textStr );
	}
      }
    }
  }
  if (str)
    XtFree( str );
}



static void historyTest( Widget w, XtPointer clientData, XmPushButtonCallbackStruct * cbs ) {

  SgFinderWidget ft = (SgFinderWidget)clientData;

  {
    printf("Clearing the history list...");
    SgFinderClearHistory( ft );
    printf("done.\n");
  }

  {
    char item[100];
    int i, maxItems = 12;

    printf("Adding %d individual items to the history list...", maxItems);
    for (i = 0; i < maxItems; i++) {
      SgFinderAddHistoryItem( ft, myItems[ i ] );
    }
    printf("done.\n");
  }

  {
    char * text;
    printf("Getting the current text...");
    text = SgFinderGetTextString( ft );
    printf("done.\n");
    printf("The current text is : %s\n", text);
    if (text)
      XtFree( text );
  }

  {
    printf("Setting the current text...");
    SgFinderSetTextString( ft, "/The/new/text/as/set/by a function" );
    printf("done.\n");
  }


  {
    SgDynaMenuWidget dm = (SgDynaMenuWidget) SgFinderGetChild( ft, SgFINDER_HISTORY_MENUBAR );
    if (dm) {
      int num = 7;
      XmString * guys = (XmString *)XtMalloc( sizeof(XmString) * num);
      int i;
      printf(" Making a list of %d guys\n", num);
      for (i = 0; i < num; i++)
	guys[ i] = XmStringCreateLocalized( newItems[ i ] );
    
      printf("Setting the history with SetValues...");
      /* Setting a list with more than maxHistoryItems in it is an error  */
      XtVaSetValues( (Widget)dm,
		    SgNhistoryListItems, guys,
		    SgNhistoryListItemCount, num,
		    NULL);
      for (i = 0; i < num; i++) {
	if (guys[ i ])
	  XmStringFree( guys[ i] );
      }

      if (guys)
	XtFree( (char *)guys );
      printf("done.\n");
  }
  }


}

static void valueChangeCB( Widget w, XtPointer clientData, XmAnyCallbackStruct * cbs) {
/*  printf("App value change callback\n"); */
}


static void activateCB( Widget w, XtPointer clientData, XmAnyCallbackStruct * cbs) {
/*  printf("App activate callback\n"); */
}

main( int argc, char * argv[] ) {

  Widget toplevel, rc, rch, label, exitB, test1, ft;
  XtAppContext app;
  XmString * list;
  int listSize, i;

  XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);

  toplevel = XtVaAppInitialize( &app, "Demos", NULL, 0, &argc, argv, NULL, NULL);

  assert( toplevel );
  rc = XtVaCreateWidget( "rc",
			xmRowColumnWidgetClass, toplevel,
			NULL);
  rch = XtVaCreateManagedWidget( "rc",
			        xmRowColumnWidgetClass, rc,
				XmNresizeWidth, False,
				XmNresizeHeight, True,
			        NULL);

  /* create the original list for the historyMenu */
  listSize = XtNumber( items );
  list = (XmString *)XtMalloc( sizeof(XmString) * listSize);
  for (i = 0; i < listSize; i++)
    list[ i ] = XmStringCreateLocalized( items[ i ] );

  assert( rc );
  
  label = XtVaCreateManagedWidget( "Open From File:",
				  xmLabelWidgetClass, rch,
				  NULL);

  ft = XtVaCreateManagedWidget("ft",
			       sgFinderWidgetClass, rch,
			       SgNhistoryListItems, list,
			       SgNhistoryListItemCount, listSize,
			       SgNsetTextSectionFunc, mySetTextSection,
			       NULL);

  
  for (i = 0; i < listSize; i++)
    if (list[ i ])
      XmStringFree(list[ i ]);
  if (list)
    XtFree( (char *)list );


  test1 = XtVaCreateManagedWidget("Resource Test",
				  xmPushButtonWidgetClass, rc,
				  NULL);

  exitB = XtVaCreateManagedWidget("Exit",
				  xmPushButtonWidgetClass, rc,
				  NULL);


  XtAddCallback( exitB, XmNactivateCallback, (XtCallbackProc)exit, NULL);
  XtAddCallback( test1, XmNactivateCallback, (XtCallbackProc)historyTest, ft);
  XtAddCallback( ft, XmNvalueChangedCallback, (XtCallbackProc)valueChangeCB, ft);
  XtAddCallback( ft, XmNactivateCallback, (XtCallbackProc)activateCB, ft);

  XtManageChild( rc );
  XtRealizeWidget( toplevel );
  XtAppMainLoop( app );

}



