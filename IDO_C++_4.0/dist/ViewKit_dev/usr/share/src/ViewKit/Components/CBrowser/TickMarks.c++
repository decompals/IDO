#include <stdio.h>
#include <Vk/VkTickMarks.h>
#include <Xm/Form.h>
#include <Xm/Scale.h>

static int count;
static Arg args[10];

class TickMarks : public VkComponent {
public:
  TickMarks(const char *name, Widget parent);
  ~TickMarks();

protected:
  VkTickMarks *tick1, *tick2;
  Widget scale1, scale2;
};

TickMarks::TickMarks(const char *name, Widget parent)
: VkComponent(name)
{
  count = 0;
  XtSetArg(args[count], XmNwidth, 200);  count++;
  XtSetArg(args[count], XmNheight, 200);  count++;
  _baseWidget = XmCreateForm(parent, (char *) name, args, count);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  scale1 = XmCreateScale(_baseWidget, "scale1", args, count);
  XtManageChild(scale1);

  tick1 = new VkTickMarks("tick1", _baseWidget);
  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNrightWidget, scale1);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  XtSetValues(tick1->baseWidget(), args, count);
  tick1->show();

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  scale2 = XmCreateScale(_baseWidget, "scale2", args, count);
  XtManageChild(scale2);

  tick2 = new VkTickMarks("tick2", _baseWidget, False);
  tick2->setScale(50, 100, 5, 0);
  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNleftWidget, scale2);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  XtSetValues(tick2->baseWidget(), args, count);
  tick2->show();

  installDestroyHandler();
}

TickMarks::~TickMarks()
{
  delete tick1;
  delete tick2;
}

VkComponent *
createTickMarks(char *name, Widget parent)
{
  return new TickMarks(name, parent);
}
