#include <stdio.h>
#include <Vk/VkRepeatButton.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>

static int count;
static Arg args[10];

class RepeatButton : public VkComponent {
public:
  RepeatButton(const char *name, Widget parent);
  ~RepeatButton();

protected:
  void setLabelString();
  void buttonPress(VkComponent *comp, void *clientData, void *callData);

  Widget label;
  VkRepeatButton *rb;
  int num;
};

RepeatButton::RepeatButton(const char *name, Widget parent)
: VkComponent(name)
{
  num = 1;

  count = 0;
  XtSetArg(args[count], XmNorientation, XmVERTICAL);  count++;
  XtSetArg(args[count], XmNpacking, XmPACK_TIGHT);  count++;
  XtSetArg(args[count], XmNentryAlignment, XmALIGNMENT_CENTER);  count++;
  _baseWidget = XmCreateRowColumn(parent, (char *) name, args, count);

  count = 0;
  label = XmCreateLabelGadget(_baseWidget, "label", args, count);
  XtManageChild(label);

  rb = new VkRepeatButton("repeatButton", _baseWidget, RB_arrowButton);
  rb->show();
  VkAddCallbackMethod(VkRepeatButton::buttonCallback, rb, this,
		      RepeatButton::buttonPress, NULL);

  setLabelString();

  installDestroyHandler();
}

RepeatButton::~RepeatButton()
{
  delete rb;
}

void
RepeatButton::setLabelString()
{
  XmString xs;
  char str[256];

  sprintf(str, "%d", num);
  xs = XmStringCreateSimple(str);
  count = 0;
  XtSetArg(args[count], XmNlabelString, xs);  count++;
  XtSetValues(label, args, count);
  XmStringFree(xs);
}

void
RepeatButton::buttonPress(VkComponent *, void *, void *)
{
  num++;
  setLabelString();
}

VkComponent *
createRepeatButton(char *name, Widget parent)
{
  return new RepeatButton(name, parent);
}
