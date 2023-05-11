/*
 * Mytest.c --
 * create and manage a dial widget.
 * Test its resource settings through menu/button actions.
 */

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/DialogS.h>
#include <Xm/ToggleB.h>
#include <Xm/LabelG.h>
#include <Xm/TextF.h>
#include <Sgm/Dial.h>

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
 * Support for spinning the dial automatically.
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
static void T_SetVisual();
static void T_SetSensitive();
static void T_SetMax();
static void T_SetMin();
static void T_SetStartAngle();
static void T_SetAngleRange();
static void T_SetMarkers();
static void T_SetMarkerLength();
static void T_SetValue();
/* T_ModifyVerify is used to make sure text fields contain only digits. */
static void T_ModifyVerify();

#else

static void T_SetDragCallback(Widget w, void *client_data, void *call_data);
static void T_SetValueChangedCallback(Widget w, void *client_data,
				      void *call_data);
static void T_SetVisual(Widget w, void *client_data, void *call_data);
static void T_SetSensitive(Widget w, void *client_data, void *call_data);
static void T_SetMax(Widget w, void *client_data, void *call_data);
static void T_SetMin(Widget w, void *client_data, void *call_data);
static void T_SetStartAngle(Widget w, void *client_data, void *call_data);
static void T_SetAngleRange(Widget w, void *client_data, void *call_data);
static void T_SetMarkers(Widget w, void *client_data, void *call_data);
static void T_SetMarkerLength(Widget w, void *client_data, void *call_data);
static void T_SetValue(Widget w, void *client_data, void *call_data);
/* T_ModifyVerify is used to make sure text fields contain only digits. */
static void T_ModifyVerify(Widget w, void *client_data, void *call_data);

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
  toplevel = CreateMainWindow(&app, "Dialtest", argc, argv);
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
  Widget visual_toggle = (Widget)NULL;
  Widget insensitive_toggle = (Widget)NULL;
  Widget max_label = (Widget)NULL;
  Widget max_text = (Widget)NULL;
  Widget min_label = (Widget)NULL;
  Widget min_text = (Widget)NULL;
  Widget start_angle_label = (Widget)NULL;
  Widget start_angle_text = (Widget)NULL;
  Widget angle_range_label = (Widget)NULL;
  Widget angle_range_text = (Widget)NULL;
  Widget markers_label = (Widget)NULL;
  Widget markers_text = (Widget)NULL;
  Widget marker_length_label = (Widget)NULL;
  Widget marker_length_text = (Widget)NULL;
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

  /* Toggle button for visual */
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftWidget, vc_toggle); ac++;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString,
	   XmStringCreateSimple("Pointer-type?")); ac++;
  visual_toggle = XmCreateToggleButton(form, "visual", args, ac);
  XtAddCallback(visual_toggle, XmNvalueChangedCallback, T_SetVisual,
		NULL);
  XtManageChild(visual_toggle);
  ac = 0;

  /* Toggle button for insensitive */
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftWidget, vc_toggle); ac++;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNtopWidget, visual_toggle); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString,
	   XmStringCreateSimple("Insensitive?")); ac++;
  insensitive_toggle = XmCreateToggleButton(form, "insensitive", args, ac);
  XtAddCallback(insensitive_toggle, XmNvalueChangedCallback, T_SetSensitive,
		NULL);
  XtManageChild(insensitive_toggle);
  ac = 0;

  /* Label and text field for maximum */
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNtopWidget, insensitive_toggle); ac++;
  XtSetArg(args[ac], XmNtopOffset, 10); ac++;
  XtSetArg(args[ac], XmNlabelString, XmStringCreateSimple("Maximum")); ac++;
  max_label = XmCreateLabelGadget(form, "max_label", args, ac);
  XtManageChild(max_label);
  ac = 0;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 250); ac++;
  XtSetArg(args[ac], XmNtopWidget, insensitive_toggle); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  max_text = XmCreateTextField(form, "max_text", args, ac);
  XtAddCallback(max_text, XmNmodifyVerifyCallback, T_ModifyVerify, NULL);
  XtAddCallback(max_text, XmNactivateCallback, T_SetMax, NULL);
  XtManageChild(max_text);
  ac = 0;

  /* Label and text field for minimum */
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
  XtAddCallback(min_text, XmNactivateCallback, T_SetMin, NULL);
  XtManageChild(min_text);
  ac = 0;

  /* Label and text field for start angle */
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNtopWidget, min_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString,XmStringCreateSimple("Start Angle")); ac++;
  start_angle_label = XmCreateLabelGadget(form, "start_angle_label", args, ac);
  XtManageChild(start_angle_label);
  ac = 0;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 250); ac++;
  XtSetArg(args[ac], XmNtopWidget, min_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  start_angle_text = XmCreateTextField(form, "start_angle_text", args, ac);
  XtAddCallback(start_angle_text, XmNmodifyVerifyCallback, T_ModifyVerify,
		NULL);
  XtAddCallback(start_angle_text, XmNactivateCallback, T_SetStartAngle, NULL);
  XtManageChild(start_angle_text);
  ac = 0;

  /* Label and text field for angle range */
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNtopWidget, start_angle_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString,XmStringCreateSimple("Angle Range")); ac++;
  angle_range_label = XmCreateLabelGadget(form, "angle_range_label", args, ac);
  XtManageChild(angle_range_label);
  ac = 0;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 250); ac++;
  XtSetArg(args[ac], XmNtopWidget, start_angle_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  angle_range_text = XmCreateTextField(form, "angle_range_text", args, ac);
  XtAddCallback(angle_range_text, XmNmodifyVerifyCallback, T_ModifyVerify, NULL);
  XtAddCallback(angle_range_text, XmNactivateCallback, T_SetAngleRange, NULL);
  XtManageChild(angle_range_text);
  ac = 0;

  /* Label and text field for markers */
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNtopWidget, angle_range_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString, XmStringCreateSimple("Number of Markers")); ac++;
  markers_label = XmCreateLabelGadget(form, "markers_label", args, ac);
  XtManageChild(markers_label);
  ac = 0;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 250); ac++;
  XtSetArg(args[ac], XmNtopWidget, angle_range_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  markers_text = XmCreateTextField(form, "markers_text", args, ac);
  XtAddCallback(markers_text, XmNmodifyVerifyCallback, T_ModifyVerify, NULL);
  XtAddCallback(markers_text, XmNactivateCallback, T_SetMarkers, NULL);
  XtManageChild(markers_text);
  ac = 0;

  /* Label and text field for marker length */
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNtopWidget, markers_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString, XmStringCreateSimple("Marker Length")); ac++;
  marker_length_label = XmCreateLabelGadget(form, "marker_length_label", args, ac);
  XtManageChild(marker_length_label);
  ac = 0;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 250); ac++;
  XtSetArg(args[ac], XmNtopWidget, markers_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  marker_length_text = XmCreateTextField(form, "marker_length_text", args, ac);
  XtAddCallback(marker_length_text, XmNmodifyVerifyCallback, T_ModifyVerify, NULL);
  XtAddCallback(marker_length_text, XmNactivateCallback, T_SetMarkerLength, NULL);
  XtManageChild(marker_length_text);
  ac = 0;

  /* Label and text field for value */
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 5); ac++;
  XtSetArg(args[ac], XmNtopWidget, marker_length_text); ac++;
  XtSetArg(args[ac], XmNtopOffset, 5); ac++;
  XtSetArg(args[ac], XmNlabelString, XmStringCreateSimple("Value")); ac++;
  value_label = XmCreateLabelGadget(form, "value_label", args, ac);
  XtManageChild(value_label);
  ac = 0;
  XtSetArg(args[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
  XtSetArg(args[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
  XtSetArg(args[ac], XmNleftOffset, 250); ac++;
  XtSetArg(args[ac], XmNtopWidget, marker_length_text); ac++;
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
   * Do not set any of the dial-specific resources.
   */

  childA = SgCreateDial(form, "A", argsA, ac);
  XtManageChild(childA);

  XtRealizeWidget(dialog);
  XtManageChild(dialog);

  return dialog;
}

void ValChangedCallback(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
  SgDialCallbackStruct *cbs = (SgDialCallbackStruct *) call_data;
  XtPointer keep_compiler_happy = call_data;

  if (cbs != NULL) {
    printf("New value %d.\n", cbs->position);
  }
}

void DragCallback(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
  SgDialCallbackStruct *cbs = (SgDialCallbackStruct *) call_data;
  XtPointer keep_compiler_happy = call_data;

  if (cbs != NULL) {
    printf("Drag to %d.\n", cbs->position);
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
 * that is used / called by the dial widget itself.
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
  XtRemoveAllCallbacks(childA, XmNvalueChangedCallback);
  if (cs->set) {
    XtAddCallback(childA, XmNvalueChangedCallback, ValChangedCallback, NULL);
  }
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_SetVisual(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetVisual(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  Arg args[1];
  XmToggleButtonCallbackStruct *cs = (XmToggleButtonCallbackStruct *)call_data;
  if (cs->set) {
    /* Toggle button set means pointer visual. */
    XtSetArg(args[0], SgNdialVisual, SgPOINTER);
  }
  else {
    /* Toggle button not set means knob visual. */
    XtSetArg(args[0], SgNdialVisual, SgKNOB);
  }
  XtSetValues(childA, args, 1);
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_SetSensitive(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetSensitive(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  Arg args[1];
  XmToggleButtonCallbackStruct *cs = (XmToggleButtonCallbackStruct *)call_data;
  if (cs->set) {
    /* Toggle button set means insensitive dial. */
    XtSetArg(args[0], XmNsensitive, FALSE);
  }
  else {
    /* Toggle button not set means sensitive dial. */
    XtSetArg(args[0], XmNsensitive, TRUE);
  }
  XtSetValues(childA, args, 1);
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_SetMax(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetMax(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  Arg args[1];
  char *text_value = NULL;
  int max_value;
  XtSetArg(args[0], XmNvalue, &text_value);
  XtGetValues(w, args, 1);
  max_value = atoi(text_value);
  XtSetArg(args[0], XmNmaximum, max_value);
  XtSetValues(childA, args, 1);
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_SetMin(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetMin(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  Arg args[1];
  char *text_value = NULL;
  int min_value;
  XtSetArg(args[0], XmNvalue, &text_value);
  XtGetValues(w, args, 1);
  min_value = atoi(text_value);
  XtSetArg(args[0], XmNminimum, min_value);
  XtSetValues(childA, args, 1);
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_SetStartAngle(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetStartAngle(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  Arg args[1];
  char *text_value = NULL;
  int start_angle;
  XtSetArg(args[0], XmNvalue, &text_value);
  XtGetValues(w, args, 1);
  start_angle = atoi(text_value);
  XtSetArg(args[0], SgNstartAngle, start_angle);
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
T_SetMarkers(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetMarkers(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  Arg args[1];
  char *text_value = NULL;
  int markers;
  XtSetArg(args[0], XmNvalue, &text_value);
  XtGetValues(w, args, 1);
  markers = atoi(text_value);
  XtSetArg(args[0], SgNdialMarkers, markers);
  XtSetValues(childA, args, 1);
  client_data = call_data = (void *)w;  /* Quiet compiler warnings */
}

static void
#ifdef _NO_PROTO
T_SetMarkerLength(w, client_data, call_data)
     Widget w;
     void *client_data;
     void *call_data;
#else
T_SetMarkerLength(Widget w, void *client_data, void *call_data)
#endif /* _NO_PROTO */
{
  Arg args[1];
  char *text_value = NULL;
  int marker_length;
  XtSetArg(args[0], XmNvalue, &text_value);
  XtGetValues(w, args, 1);
  marker_length = atoi(text_value);
  XtSetArg(args[0], SgNmarkerLength, marker_length);
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
