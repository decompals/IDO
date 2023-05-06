#include <stdio.h>
#include <Vk/VkModified.h>
#include <Vk/VkComponent.h>
#include <Xm/Form.h>
#include <Xm/Text.h>

static int count;
static Arg args[10];

class Modified : public VkComponent {
public:
  Modified(const char *name, Widget parent);
  ~Modified();

protected:
  static void text_activate(Widget w, XtPointer clientData,
			    XtPointer callData);

  Widget text;
  VkModifiedAttachment *mod;
};

Modified::Modified(const char *name, Widget parent)
: VkComponent(name)
{
  count = 0;
  _baseWidget = XmCreateForm(parent, (char *) name, args, count);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  text = XmCreateText(_baseWidget, "text", args, count);
  XtManageChild(text);
  XtAddCallback(text, XmNactivateCallback, Modified::text_activate, NULL);

  mod = new VkModifiedAttachment();
  mod->attach(text);
  mod->show();

  mod->setValue("one");
  mod->setValue("two");

  installDestroyHandler();
}

Modified::~Modified()
{
  delete mod;
}

void
Modified::text_activate(Widget w, XtPointer, XtPointer)
{
  XmTextSetInsertionPosition(w, 0);
}

VkComponent *
createModified(char *name, Widget parent)
{
  return new Modified(name, parent);
}
