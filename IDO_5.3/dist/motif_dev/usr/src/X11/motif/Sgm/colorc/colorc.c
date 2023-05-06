/* 
 * File: main.c++
 *
 * Written by: Ofer Ben-Shachar, March 1993
 *
 * Simple test program for the new selection API. 
 * Enables selecting widget name and class and pasting to a list widget.
 */

#include <stdio.h>
#include <string.h>
#include <X11/IntrinsicP.h>
#include <Xm/Xm.h>
#include <Mrm/MrmPublic.h>

#include <X11/Xatom.h>
#include <Xm/AtomMgr.h>
#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>

#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/LabelG.h>
#include <Xm/SeparatoG.h>
#include <Sgm/ColorC.h>

#define MAX_SELECTION_STR_SIZE 256

static char *mrm_filename_list[] = {"colorc.uid"};

static void PopupColorCCallback(Widget w, void *client_data, void *call_data);
static void CloseCallback(Widget w, void *client_data, void *call_data);

static MrmRegisterArg names_array[] =
{
  {"PopupColorCCallback", (caddr_t) PopupColorCCallback},
  {"CloseCallback",       (caddr_t) CloseCallback},
};


static Widget color_chooser = NULL;

static Widget main_shell;
static Widget main_form;
static Widget text_widget;
static Widget text_field_widget;

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

static void ColorSetCallback(Widget w, void *client_data, 
			     XmAnyCallbackStruct *call_data)
{
  Boolean *drag = (Boolean *) client_data;
  char    color_text[128];
  short   r, g, b;
  String  action;

  SgColorChooserGetColor(w, &r, &g, &b);
  if(call_data->reason == XmCR_OK) 
    action = "OK";
  else if(call_data->reason == XmCR_APPLY) 
    action = "APPLY";

  sprintf(color_text, "%s color values: R %d, G %d, B %d", action, r, g, b);
  XmTextFieldSetString(text_field_widget, color_text);
}

/* 
 * PopupColorCCallback
 *
 * Exit the application.
 */
static void PopupColorCCallback(Widget w, void *client_data, 
				void *call_data)
{
  Widget   form;
  Widget   label;
  XmString label_string;
  Arg      al[8];
  int      ac;

  if(color_chooser == NULL) {
    Dimension width, height;

    ac = 0;
    /* XtSetArg(al[ac], SgNwysiwyg, False); ac++; */
    color_chooser = 
      SgCreateColorChooserDialog(main_shell, "Color Chooser", al, ac);
    SgColorChooserSetColor(color_chooser, 62, 24, 111); /* Indigo color */
    XtAddCallback(color_chooser, XmNvalueChangedCallback, 
		  (XtCallbackProc) ColorChangeCallback, NULL);

    XtAddCallback(color_chooser, XmNdragCallback, 
		  (XtCallbackProc) ColorChangeCallback, NULL);
    XtAddCallback(color_chooser, XmNokCallback, 
		  (XtCallbackProc) ColorSetCallback, NULL);
    XtAddCallback(color_chooser, XmNapplyCallback, 
		  (XtCallbackProc) ColorSetCallback, NULL);


    ac = 0;
    form = XmCreateForm(color_chooser, "Form", al, ac);
#if 0
    XtManageChild(form);
#endif
    ac = 0;
    XtSetArg (al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
    XtSetArg (al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    label_string = 
      XmStringCreateLtoR("Work Area Child Widget", XmSTRING_DEFAULT_CHARSET);
    
    XtSetArg (al[ac], XmNlabelString, label_string); ac++;
    XtSetArg (al[ac], XmNrecomputeSize, False); ac++;
    label = XmCreateLabel(form, "Label", al, ac);
    XtManageChild(label);

    XtManageChild(color_chooser);

#if 0
    ac = 0;
    XtSetArg (al[ac], XmNwidth, &width); ac++;
    XtSetArg (al[ac], XmNheight, &height); ac++;
    XtGetValues(XtParent(color_chooser), al, ac);
    ac = 0;
    XtSetArg (al[ac], XmNminWidth, width); ac++;
    XtSetArg (al[ac], XmNminHeight, height); ac++;
    XtSetValues(XtParent(color_chooser), al, ac);
#endif

    ac = 0;
    XtSetArg (al[ac], SgNshowSliders, SgValue); ac++; 
    XtSetValues(color_chooser, al, ac);
  }
  else
    XtManageChild(color_chooser);
}

/* 
 * CloseCallback
 *
 * Exit the application.
 */
static void CloseCallback(Widget w, void *client_data, void *call_data)
{
  w = NULL;			/* avoid compile warning messages */
  client_data = call_data = NULL; /* avoid compile warning messages */

  exit(0);			/* Exit the application */
}


int main (int argc, char **argv)
{ 
  XtAppContext app_context;
  Display     *display;		/*  Display             */
  MrmHierarchy mrm_hierarchy;
  MrmCode      c_class;
  Arg          al[6];           /* Arg List */
  int          ac = 0;		/* Arg Count */

  MrmInitialize();
  XtToolkitInitialize ();
  app_context = XtCreateApplicationContext ();
  display = XtOpenDisplay (app_context, NULL, argv[0], "Colorc",
			   NULL, 0, &argc, argv);
  if (!display)
    {
      printf("%s: can't open display, exiting...\n", argv[0]);
      exit (-1);
    }

  /* Register converters, just in case you are really unlucky !! */
  XmRegisterConverters();
  /* String to unit type doesn't get added !! */
  XtAddConverter ( XmRString, XmRUnitType, XmCvtStringToUnitType, NULL, 0 );


  XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
  XtSetArg(al[ac], XmNargc, argc); ac++;
  XtSetArg(al[ac], XmNargv, argv); ac++;
  main_shell = XtAppCreateShell ( argv[0], "Colorc", 
				 applicationShellWidgetClass, display, 
				 al, ac );

  if ( MrmOpenHierarchy ( 1, mrm_filename_list, NULL, &mrm_hierarchy ) != MrmSUCCESS )
    {
      printf("%s: can't open mrm hierarchy, exiting...\n", argv[0]);
      exit (-1);
    }
  if ( MrmRegisterNames (names_array, XtNumber(names_array)) != MrmSUCCESS ) {
    printf("Can't register names\n");
    exit (-1);
  }

  ac = 0;
  XtSetArg(al[ac], XmNtitle, "Selection Widget Name"); ac++;
  XtSetArg(al[ac], XmNiconName, "Selection Widget Name"); ac++;
  XtSetArg(al[ac], XmNminWidth, 300); ac++;
  XtSetArg(al[ac], XmNminHeight, 200); ac++;
  XtSetValues(main_shell, al, ac);

  if(MrmFetchWidget(mrm_hierarchy, "main_form", main_shell, 
		    &main_form, &c_class) != MrmSUCCESS) {
    printf("can't fetch interface\n");;
    exit(-1);
  }

  text_widget = XtNameToWidget(main_form, "*text");
  text_field_widget = XtNameToWidget(main_form, "*text_field");
  /* Manage the main form */
  XtManageChild (main_form);

  XtRealizeWidget (main_shell);

  XtAppMainLoop(app_context);

  return (0);
}

