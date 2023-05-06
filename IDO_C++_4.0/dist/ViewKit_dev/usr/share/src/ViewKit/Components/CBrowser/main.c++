#include <stdio.h>
#include <stdlib.h>
#include "CBrowser.h"
#include <Vk/VkApp.h>

static String resources[] = {
"-*useSchemes:     			all",
"-*baseScheme:     			Base",
"-*defaultScheme:  			Lascaux",
"-*keyboardFocusPolicy:			pointer",
"-*cbrowser.Title:			ViewKit Components Browser",
"-*coverview.Title:			ViewKit Components Overview",
"-*fileMenu.labelString:			File",
"-*fileMenu.mnemonic:			F",
"-*fileMenu*showOverviewItem.labelString:	Show Overview",
"-*fileMenu*showOverviewItem.mnemonic:	S",
"-*fileMenu*quitMenuItem.labelString:	Exit",
"-*fileMenu*quitMenuItem.mnemonic:	E",
"-*form.leftOffset:			10",
"-*form.rightOffset:			10",
"-*form.topOffset:			10",
"-*form.bottomOffset:			10",
"-*clist.width:				200",
"-*frame.shadowType:			XmSHADOW_IN",
"-*frame.marginWidth:			5",
"-*frame.marginHeight:			5",
"-*display.width:				400",
"-*display.height:			300",
"-*Title:					CBrowser",
"-*clist*label.labelString:		ViewKit Component: ",
"-*clist*label.alignment:			XmALIGNMENT_BEGINNING",
"-*clist*label.recomputeSize:		False",
"-*clist*list.scrollBarDisplayPolicy:	XmSTATIC",
"-*carea*label.alignment:			XmALIGNMENT_BEGINNING",
"-*carea*label.recomputeSize:		False",
"-*carea.leftOffset:			10",
"-*grid.width:				400",
"-*grid.height:				300",
"-*grid.defaultSpacing:			2",
"-*grid*element.marginWidth:		4",
"-*grid*element.marginHeight:		4",
"-*grid*element.shadowThickness:		2",
"-*grid*element.shadowType:		XmSHADOW_ETCHED_IN",
"-*grid*element*label.alignment:		XmALIGNMENT_CENTER",
NULL,
};

class MyApp : public VkApp {
public:
  MyApp(char             *appClassName,
	int              *arg_c,
	char            **arg_v);
  ~MyApp();
};

MyApp::MyApp(char             *appClassName,
	     int              *arg_c,
	     char            **arg_v)
: VkApp(appClassName, arg_c, arg_v)
{
  setDefaultResources(baseWidget(), resources);
}

MyApp::~MyApp()
{
}

void
main(int argc, char **argv)
{
  Cbrowser *cbrowser;
  VkApp *app;

  app = new MyApp("Cbrowser", &argc, argv);

  cbrowser = new Cbrowser("cbrowser");
  cbrowser->show();

  app->run();
}
