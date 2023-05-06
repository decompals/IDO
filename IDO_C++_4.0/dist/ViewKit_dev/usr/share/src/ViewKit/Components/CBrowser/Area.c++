#include <stdio.h>
#include "Area.h"

#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/LabelG.h>

static int count;
static Arg args[10];

Carea::Carea(const char *name, Widget parent)
: VkComponent(name)
{
  count = 0;
  _baseWidget = XmCreateForm(parent, (char *) name, args, count);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  label = XmCreateLabelGadget(_baseWidget, "label", args, count);
  XtManageChild(label);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNtopWidget, label);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  frame = XmCreateFrame(_baseWidget, "frame", args, count);
  XtManageChild(frame);

  count = 0;
  display = XmCreateForm(frame, "display", args, count);
  XtManageChild(display);

  setLabel(" ");
  installDestroyHandler();
}

Carea::~Carea()
{
}

const char *
Carea::className()
{
  return "CArea";
}

/**********************************************************************/

void
Carea::setLabel(char *str)
{
  XmString xs;

  xs = XmStringCreateSimple(str);
  count = 0;
  XtSetArg(args[count], XmNlabelString, xs);  count++;
  XtSetValues(label, args, count);
  XmStringFree(xs);
}
