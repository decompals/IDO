#include <stdio.h>
#include <Vk/VkCompletionField.h>
#include <Xm/Form.h>
#include <Xm/LabelG.h>

static int count;
static Arg args[10];

class CompletionField : public VkComponent {
public:
  CompletionField(const char *name, Widget parent);
  ~CompletionField();

protected:

  Widget label;
  VkCompletionField *field;
};

CompletionField::CompletionField(const char *name, Widget parent)
: VkComponent(name)
{
  Dimension height;

  count = 0;
  _baseWidget = XmCreateForm(parent, (char *) name, args, count);

  count = 0;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  label = XmCreateLabelGadget(_baseWidget, "Month: ", args, count);
  XtManageChild(label);

  field = new VkCompletionField("field", _baseWidget);
  count = 0;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNleftWidget, label);  count++;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetValues(field->baseWidget(), args, count);
  field->add("January"); 
  field->add("january");
  field->add("February"); 
  field->add("february");
  field->add("March"); 
  field->add("march");
  field->add("April"); 
  field->add("april");
  field->add("May"); 
  field->add("may");
  field->add("June"); 
  field->add("june");
  field->add("July"); 
  field->add("july");
  field->add("August"); 
  field->add("august");
  field->add("September"); 
  field->add("september");
  field->add("October"); 
  field->add("october");
  field->add("November"); 
  field->add("november");
  field->add("December"); 
  field->add("december");
  field->show();

  count = 0;
  XtSetArg(args[count], XmNheight, &height);  count++;
  XtGetValues(field->baseWidget(), args, count);
  count = 0;
  XtSetArg(args[count], XmNheight, height);  count++;
  XtSetValues(label, args, count);

  installDestroyHandler();
}

CompletionField::~CompletionField()
{
  delete field;
}

VkComponent *
createCompletionField(char *name, Widget parent)
{
  return new CompletionField(name, parent);
}
