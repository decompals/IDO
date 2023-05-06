#include "Element.h"
#include "Data.h"

#include <Xm/Form.h>
#include <Xm/LabelG.h>
#include <Xm/SeparatoG.h>

static int count;
static Arg args[20];

Celement::Celement(const char *name, Widget parent, int componentIndex)
: VkComponent(name)
{
  XmString xs;

  count = 0;
  XtSetArg(args[count], XmNresizePolicy, XmRESIZE_NONE);  count++;
  _baseWidget = XmCreateForm(parent, (char *) name, args, count);

  xs = XmStringCreateSimple(componentInfo[componentIndex].name);
  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNlabelString, xs);  count++;
  label = XmCreateLabelGadget(_baseWidget, "label", args, count);
  XmStringFree(xs);
  XtManageChild(label);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNtopWidget, label);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNorientation, XmHORIZONTAL);  count++;
  sep = XmCreateSeparatorGadget(_baseWidget, "sep", args, count);
  XtManageChild(sep);

  component = (*(componentInfo[componentIndex].proc))("component",
						      _baseWidget);
  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNtopWidget, sep);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  XtSetValues(component->baseWidget(), args, count);
  component->show();

  installDestroyHandler();
}

Celement::~Celement()
{
  delete component;
}
