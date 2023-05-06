/*
 * Mytest.c --
 * create and manage a thumbwheel widget.
 * Test its resource settings through menu/button actions.
 */

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/DialogS.h>
#include <Xm/ToggleB.h>
#include <Xm/LabelG.h>
#include <Xm/TextF.h>
#include <Sgm/ThumbWheel.h>

/*
 * Test framework procedures and globals.
 */
#define SCROLLBAR_MAX 150

#ifdef _NO_PROTO
static void ValChangedCallback();
static void DragCallback();
static Widget CreateMainWindow();
static Widget CreateChildWindow();
#else
static void ValChangedCallback(Widget w, void *client_data, void *call_data);
static void DragCallback(Widget w, void *client_data, void *call_data);
static Widget CreateMainWindow(XtAppContext *ac, char *classname, int argc,
			       char *argv[]);
static Widget CreateChildWindow(Widget parent);
#endif /* _NO_PROTO */

Widget childA;
XtAppContext app;


/*
 * Support for spinning the wheel automatically.
 */
#define DELAY 100

#ifdef _NO_PROTO
static void TimerCbk();
#else
static void TimerCbk(void *client_data, XtIntervalId *id);
#endif /* _NO_PROTO */

Boolean going_up = TRUE;

/*
 * Callbacks used by the menus and buttons of the test framework.
 */
#ifdef _NO_PROTO

static void T_SetDragCallback();
static void T_SetValueChangedCallback();
static void T_SetMaximum();
static void T_SetMinimum();
static void T_SetHomePosition();
static void T_SetOrientation();
static void T_SetAngleRange();
static void T_SetUnitsPerRotation();
static void T_SetShowHomeButton();
static void T_SetValue();
/* T_ModifyVerify is used to make sure text fields contain only digits. */
static void T_ModifyVerify();

#else

static void T_SetDragCallback(Widget w, void *client_data,
			      void *call_data);
static void T_SetValueChangedCallback(Widget w, void *client_data,
				      void *call_data);
static void T_SetMaximum(Widget w, void *client_data,
			 void *call_data);
static void T_SetMinimum(Widget w, void *client_data,
			 void *call_data);
static void T_SetHomePosition(Widget w, void *client_data,
			      void *call_data);
static void T_SetOrientation(Widget w, void *client_data,
			     void *call_data);
static void T_SetAngleRange(Widget w, void *client_data,
			    void *call_data);
static void T_SetUnitsPerRotation(Widget w, void *client_data,
				  void *call_data);
static void T_SetShowHomeButton(Widget w, void *client_data,
				void *call_data);
static void T_SetValue(Widget w, void *client_data,
		       void *call_data);
/* T_ModifyVerify is used to make sure text fields contain only digits. */
static void T_ModifyVerify(Widget w, void *client_data,
			   void *call_data);

#endif /* _NO_PROTO */

main (argc, argv)
int argc;
char *argv[];
{
  Widget toplevel, dialog;

  /*
   * Create and realize our top level window,
   * with all the menus and buttons for user input.
   */
  toplevel = CreateMainWindow(&app, "Thumbtest", argc, argv);
  if (toplevel == (Widget)NULL) {
    printf("AppInitialize failed!\n");
    exit(1);
  }

  /*
   * Create and manage the pop-up dialog containing the
   * widget we are testing.
   */
  dialog = CreateChildWindow(toplevel);

/*  (void) XtAppAddTimeOut(app, DELAY, TimerCbk, NULL); */
  XtAppMainLoop(app);
}

Widget CreateMainWindow(XtAppContext *appc, char *classname, int argc,
			char *argv[])
{
  Widget main_w = (Widget)NULL;
  Widget form = (Widget)NULL;
  Widget drag_toggle = (Widget)NULL;
  Widget vc_toggle = (Widget)NULL;
  Widget orientation_toggle = (Widget)NULL;
  Widget home_toggle = (Widget)NULL;
  Widget max_label = (Widget)NULL;
  Widget max_text = (Widget)NULL;
  Widget min_label = (Widget)NULL;
  Widget min_text = (Widget)NULL;
  Widget angle_label = (Widget)NULL;
  Widget angle_text = (Widget)NULL;
  Widget home_label = (Widget)NULL;
  Widget home_text = (Widget)NULL;
  Widget units_label = (Widget)NULL;
  Widget units_text = (Widget)NULL;
  Widget value_label = (Widget)NULL;
  Widget value_text = (Widget)NULL;
  Arg args[25];
  int ac = 0;

  main_w = XtVaAppInitialize(appc, classname, NULL, 0, &argc, argv,
			     NULL, NULL);
  if (main_w == NULL) {
    /* Failure!  main() will produce an error message and exit. */
    return main_w;
  }

  /*
   * Set up everything inside the main window.
   */

  XtSetArg(args[ac], XmNverticalSpacing, 5); ac++;
  XtSetArg(args[ac], XmNhorizontalSpacing, 5); ac++;
  XtSetArg(args[ac], XmNmarginWidth, 5); ac++;
  form = XmCreateForm(main_w, "form", NULL, 0);
  ac = 0;

  /* Toggle button for drag callback */
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString,
	   XmStringCreateSimple("Drag Callback")); ac++;
  drag_toggle = XmCreateToggleButton(form, "drag", args, ac);
  XtAddCallback(drag_toggle, XmNvalueChangedCallback, T_SetDragCallback, NULL);
  XtManageChild(drag_toggle);
  ac = 0;

  /* Toggle button for value changed callback */
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNtopWidget, drag_toggle); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString,
	   XmStringCreateSimple("Value Changed Callback")); ac++;
  vc_toggle = XmCreateToggleButton(form, "vc", args, ac);
  XtAddCallback(vc_toggle, XmNvalueChangedCallback, T_SetValueChangedCallback,
		NULL);
  XtManageChild(vc_toggle);
  ac = 0;

  /* Toggle button for orientation */
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftWidget, vc_toggle); ac++;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString,
	   XmStringCreateSimple("Horizontal?")); ac++;
  orientation_toggle = XmCreateToggleButton(form, "orientation", args, ac);
  XtAddCallback(orientation_toggle, XmNvalueChangedCallback, T_SetOrientation,
		NULL);
  XtManageChild(orientation_toggle);
  ac = 0;

  /* Toggle button for home */
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftWidget, vc_toggle); ac++;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNtopWidget, orientation_toggle); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString,
	   XmStringCreateSimple("Show Home Button")); ac++;
  home_toggle = XmCreateToggleButton(form, "home", args, ac);
  XtAddCallback(home_toggle, XmNvalueChangedCallback, T_SetShowHomeButton,
		NULL);
  XtManageChild(home_toggle);
  ac = 0;

  /* Label and text field for maximum value */
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNtopWidget, home_toggle); ac++;
  XtSetArg(args[ac], XmNtopOffset, 10); ac++;
  XtSetArg(args[ac], XmNlabelString, XmStringCreateSimple("Maximum")); ac++;
  max_label = XmCreateLabelGadget(form, "max_label", args, ac);
  XtManageChild(max_label);
  ac = 0;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 250); ac++;
  XtSetArg(args[ac], XmNtopWidget, home_toggle); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  max_text = XmCreateTextField(form, "max_text", args, ac);
  XtAddCallback(max_text, XmNmodifyVerifyCallback, T_ModifyVerify, NULL);
  XtAddCallback(max_text, XmNactivateCallback, T_SetMaximum, NULL);
  XtManageChild(max_text);
  ac = 0;

  /* Label and text field for minimum value */
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNtopWidget, max_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString, XmStringCreateSimple("Minimum")); ac++;
  min_label = XmCreateLabelGadget(form, "min_label", args, ac);
  XtManageChild(min_label);
  ac = 0;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 250); ac++;
  XtSetArg(args[ac], XmNtopWidget, max_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  min_text = XmCreateTextField(form, "min_text", args, ac);
  XtAddCallback(min_text, XmNmodifyVerifyCallback, T_ModifyVerify, NULL);
  XtAddCallback(min_text, XmNactivateCallback, T_SetMinimum, NULL);
  XtManageChild(min_text);
  ac = 0;

  /* Label and text field for angle range*/
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNtopWidget, min_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString, XmStringCreateSimple("Angle Range")); ac++;
  angle_label = XmCreateLabelGadget(form, "angle_label", args, ac);
  XtManageChild(angle_label);
  ac = 0;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 250); ac++;
  XtSetArg(args[ac], XmNtopWidget, min_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  angle_text = XmCreateTextField(form, "angle_text", args, ac);
  XtAddCallback(angle_text, XmNmodifyVerifyCallback, T_ModifyVerify, NULL);
  XtAddCallback(angle_text, XmNactivateCallback, T_SetAngleRange, NULL);
  XtManageChild(angle_text);
  ac = 0;

  /* Label and text field for home position */
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNtopWidget, angle_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString, XmStringCreateSimple("Home Position")); ac++;
  home_label = XmCreateLabelGadget(form, "home_label", args, ac);
  XtManageChild(home_label);
  ac = 0;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 250); ac++;
  XtSetArg(args[ac], XmNtopWidget, angle_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  home_text = XmCreateTextField(form, "home_text", args, ac);
  XtAddCallback(home_text, XmNmodifyVerifyCallback, T_ModifyVerify, NULL);
  XtAddCallback(home_text, XmNactivateCallback, T_SetHomePosition, NULL);
  XtManageChild(home_text);
  ac = 0;

  /* Label and text field for units per rotation */
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNtopWidget, home_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString, XmStringCreateSimple("Units Per Rotation")); ac++;
  units_label = XmCreateLabelGadget(form, "units_label", args, ac);
  XtManageChild(units_label);
  ac = 0;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 250); ac++;
  XtSetArg(args[ac], XmNtopWidget, home_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  units_text = XmCreateTextField(form, "units_text", args, ac);
  XtAddCallback(units_text, XmNmodifyVerifyCallback, T_ModifyVerify, NULL);
  XtAddCallback(units_text, XmNactivateCallback, T_SetUnitsPerRotation, NULL);
  XtManageChild(units_text);
  ac = 0;

  /* Label and text field for value */
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNtopWidget, units_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString, XmStringCreateSimple("Value")); ac++;
  value_label = XmCreateLabelGadget(form, "value_label", args, ac);
  XtManageChild(value_label);
  ac = 0;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 250); ac++;
  XtSetArg(args[ac], XmNtopWidget, units_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  value_text = XmCreateTextField(form, "value_text", args, ac);
  XtAddCallback(value_text, XmNmodifyVerifyCallback, T_ModifyVerify, NULL);
  XtAddCallback(value_text, XmNactivateCallback, T_SetValue, NULL);
  XtManageChild(value_text);
  ac = 0;

  /*
   * Now that it's all set up, manage everything
   */
  XtManageChild(form);
  XtRealizeWidget(main_w);
  return main_w;
}

Widget CreateChildWindow(Widget parent)
{
  Widget dialog, form;
  Arg argsA[25];
  int ac = 0;

  XtSetArg(argsA[ac], XmNallowShellResize, TRUE); ac++;
  dialog = XmCreateDialogShell(parent, "Child", argsA, ac);
  ac = 0;
  form = XmCreateForm(dialog, "Form", NULL, 0);
  XtManageChild(form);

  /* Set up arguments for our widget. */
  XtSetArg(argsA[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(argsA[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
  XtSetArg(argsA[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
  XtSetArg(argsA[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;

  /*
   * We use all-default settings to begin with.
   * Do not set any of the thumbwheel-specific resources.
   */

  childA = SgCreateThumbWheel(form, "A", argsA, ac);
  XtManageChild(childA);

  XtRealizeWidget(dialog);
  XtManageChild(dialog);

  return dialog;
}

void ValChangedCallback(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
  SgThumbWheelCallbackStruct *cbs = (SgThumbWheelCallbackStruct *) call_data;
  XtPointer keep_compiler_happy = call_data;

  if (cbs != NULL) {
    printf("New value %d.\n", cbs->value);
  }
}

void DragCallback(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
  SgThumbWheelCallbackStruct *cbs = (SgThumbWheelCallbackStruct *) call_data;
  XtPointer keep_compiler_happy = call_data;

  if (cbs != NULL) {
    printf("Drag to %d.\n", cbs->value);
  }
}

void TimerCbk (void *client_data, XtIntervalId *id)
{
  int value;
  Arg args[1];

  XtSetArg(args[0], XmNvalue, &value);
  XtGetValues(childA, args, 1);

  if (value == SCROLLBAR_MAX) {
    going_up = FALSE;
  }
  if (value == 0) {
    going_up = TRUE;
  }
  if (going_up) {
    value += 5;
  }
  else {
    value -= 5;
  }

  XtSetArg(args[0], XmNvalue, value);
  XtSetValues(childA, args, 1);
/*  fprintf(stderr, " %d", value); fflush(stderr); */

  (void) XtAppAddTimeOut(app, DELAY, TimerCbk, NULL);
  client_data = (void *)id;  /* Quiet compiler warning messages */
}

/*
 * Callbacks used by the menus and buttons of the test framework.
 * The prefix T_ indicates it is a test program callback and NOT one
 * that is used / called by the thumbwheel widget itself.
 */

static void
#ifdef _NO_PROTO
T_SetDragCallback(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetDragCallback(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  XmToggleButtonCallbackStruct *cs = (XmToggleButtonCallbackStruct *)call_data;
  XtRemoveAllCallbacks(childA, XmNdragCallback);
  if (cs->set) {
    XtAddCallback(childA, XmNdragCallback, DragCallback, NULL);
  }
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_SetValueChangedCallback(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetValueChangedCallback(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  XmToggleButtonCallbackStruct *cs = (XmToggleButtonCallbackStruct *)call_data;
  XtRemoveAllCallbacks(childA, XmNdragCallback);
  if (cs->set) {
    XtAddCallback(childA, XmNvalueChangedCallback, ValChangedCallback, NULL);
  }
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_SetMaximum(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetMaximum(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  Arg args[1];
  char *text_value = NULL;
  int max_bound;
  XtSetArg(args[0], XmNvalue, &text_value);
  XtGetValues(w, args, 1);
  max_bound = atoi(text_value);
  XtSetArg(args[0], XmNmaximum, max_bound);
  XtSetValues(childA, args, 1);
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_SetMinimum(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetMinimum(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  Arg args[1];
  char *text_value = NULL;
  int min_bound;
  XtSetArg(args[0], XmNvalue, &text_value);
  XtGetValues(w, args, 1);
  min_bound = atoi(text_value);
  XtSetArg(args[0], XmNminimum, min_bound);
  XtSetValues(childA, args, 1);
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_SetHomePosition(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetHomePosition(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  Arg args[1];
  char *text_value = NULL;
  int home;
  XtSetArg(args[0], XmNvalue, &text_value);
  XtGetValues(w, args, 1);
  home = atoi(text_value);
  XtSetArg(args[0], SgNhomePosition, home);
  XtSetValues(childA, args, 1);
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_SetOrientation(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetOrientation(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  Arg args[1];
  XmToggleButtonCallbackStruct *cs = (XmToggleButtonCallbackStruct *)call_data;
  if (cs->set) {
    /* Toggle button set means horizontal orientation. */
    XtSetArg(args[0], XmNorientation, XmHORIZONTAL);
  }
  else {
    /* Toggle button not set means vertical orientation. */
    XtSetArg(args[0], XmNorientation, XmVERTICAL);
  }
  XtSetValues(childA, args, 1);
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_SetAngleRange(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetAngleRange(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  Arg args[1];
  char *text_value = NULL;
  int angle_range;
  XtSetArg(args[0], XmNvalue, &text_value);
  XtGetValues(w, args, 1);
  angle_range = atoi(text_value);
  XtSetArg(args[0], SgNangleRange, angle_range);
  XtSetValues(childA, args, 1);
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_SetUnitsPerRotation(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetUnitsPerRotation(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  Arg args[1];
  char *text_value = NULL;
  int units_per;
  XtSetArg(args[0], XmNvalue, &text_value);
  XtGetValues(w, args, 1);
  units_per = atoi(text_value);
  XtSetArg(args[0], SgNunitsPerRotation, units_per);
  XtSetValues(childA, args, 1);
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_SetShowHomeButton(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetShowHomeButton(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  Arg args[1];
  XmToggleButtonCallbackStruct *cs = (XmToggleButtonCallbackStruct *)call_data;
  if (cs->set) {
    /* Toggle button set means show home button. */
    XtSetArg(args[0], SgNshowHomeButton, TRUE);
  }
  else {
    /* Toggle button not set means don't show home button. */
    XtSetArg(args[0], SgNshowHomeButton, FALSE);
  }
  XtSetValues(childA, args, 1);
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_SetValue(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetValue(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  Arg args[1];
  char *text_value = NULL;
  int value;
  XtSetArg(args[0], XmNvalue, &text_value);
  XtGetValues(w, args, 1);
  value = atoi(text_value);
  XtSetArg(args[0], XmNvalue, value);
  XtSetValues(childA, args, 1);
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_ModifyVerify(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_ModifyVerify(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  int i;
  XmTextVerifyCallbackStruct *vcs = (XmTextVerifyCallbackStruct *)call_data;
  if ((vcs->text->ptr == NULL) || (vcs->text->length == 0)) {
    return;
  }

  for (i = 0; i < vcs->text->length; i++) {
    if (!(isdigit(vcs->text->ptr[i]))) {
      vcs->doit = False;
    }
  }

  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}
