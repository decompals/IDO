#include <stdio.h>
#include <Vk/VkPrefItem.h>
#include <Xm/Form.h>

static int count;
static Arg args[10];

class PrefItem : public VkComponent {
public:
  PrefItem(const char *name, Widget parent);
  ~PrefItem();

protected:

  VkPrefGroup *group;
  VkPrefText *text;
  VkPrefToggle *toggle;
  VkPrefOption *option;
};

PrefItem::PrefItem(const char *name, Widget parent)
: VkComponent(name)
{
  XmString xs;

  count = 0;
  _baseWidget = XmCreateForm(parent, (char *) name, args, count);

  text = new VkPrefText("Text");
  toggle = new VkPrefToggle("Toggle");
  option = new VkPrefOption("Option", 2);
  option->setLabel(0, "one");
  option->setLabel(1, "two");

  group = new VkPrefGroup("prefGroup");
  group->addItem(text);
  group->addItem(toggle);
  group->addItem(option);

  group->build(_baseWidget);
  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetValues(group->baseWidget(), args, count);
  group->show();

  xs = XmStringCreateSimple("Text: ");
  count = 0;
  XtSetArg(args[count], XmNlabelString, xs);  count++;
  XtSetValues(text->labelWidget(), args, count);
  XmStringFree(xs);

  xs = XmStringCreateSimple("Toggle: ");
  count = 0;
  XtSetArg(args[count], XmNlabelString, xs);  count++;
  XtSetValues(toggle->labelWidget(), args, count);
  XmStringFree(xs);

  xs = XmStringCreateSimple("Option: ");
  count = 0;
  XtSetArg(args[count], XmNlabelString, xs);  count++;
  XtSetValues(option->labelWidget(), args, count);
  XmStringFree(xs);

  installDestroyHandler();
}

PrefItem::~PrefItem()
{
  group->deleteChildren();
  delete group;
}

VkComponent *
createPrefItem(char *name, Widget parent)
{
  return new PrefItem(name, parent);
}
