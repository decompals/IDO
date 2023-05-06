#include <stdio.h>
#include <Vk/VkResizer.h>
#include <Vk/VkComponent.h>
#include <Xm/DrawingA.h>
#include <Xm/PushB.h>

static int count;
static Arg args[10];

class Resizer : public VkComponent {
public:
  Resizer(const char *name, Widget parent);
  ~Resizer();

protected:
  void buttonActivate();
  void setLabel();
  void stateChanged(VkComponent *comp, void *clientData, void *callData);

  static void button_activate(Widget w, XtPointer clientData,
			      XtPointer callData);

  Widget button;
  VkResizer *resizer;
  Boolean shown;
};

Resizer::Resizer(const char *name, Widget parent)
: VkComponent(name)
{
  XmString xs;

  shown = True;

  count = 0;
  XtSetArg(args[count], XmNmarginWidth, 0);  count++;
  XtSetArg(args[count], XmNmarginHeight, 0);  count++;
  XtSetArg(args[count], XmNresizePolicy, XmRESIZE_NONE);  count++;
  _baseWidget = XmCreateDrawingArea(parent, (char *) name, args, count);

  xs = XmStringCreateSimple("000x000+000+000");
  count = 0;
  XtSetArg(args[count], XmNrecomputeSize, False);  count++;
  XtSetArg(args[count], XmNalignment, XmALIGNMENT_CENTER);  count++;
  XtSetArg(args[count], XmNlabelString, xs);  count++;
  XtSetArg(args[count], XmNx, 20);  count++;
  XtSetArg(args[count], XmNy, 20);  count++;
  button = XmCreatePushButton(_baseWidget, "button", args, count);
  XmStringFree(xs);
  XtManageChild(button);
  XtAddCallback(button, XmNactivateCallback, Resizer::button_activate,
		(XtPointer) this);

  resizer = new VkResizer();
  resizer->attach(button);
  resizer->show();
  VkAddCallbackMethod(VkResizer::stateChangedCallback, resizer, this,
		      Resizer::stateChanged, NULL);

  setLabel();

  installDestroyHandler();
}

Resizer::~Resizer()
{
  delete resizer;
}

void
Resizer::buttonActivate()
{
  if (shown) {
    resizer->hide();
  } else {
    resizer->attach(button);
    resizer->show();
  }
  shown = !shown;
}

void
Resizer::setLabel()
{
  Position x, y;
  Dimension width, height;
  char str[256];
  XmString xs;

  count = 0;
  XtSetArg(args[count], XmNx, &x);  count++;
  XtSetArg(args[count], XmNy, &y);  count++;
  XtSetArg(args[count], XmNwidth, &width);  count++;
  XtSetArg(args[count], XmNheight, &height);  count++;
  XtGetValues(button, args, count);
  sprintf(str, "%dx%d+%d+%d", width, height, x, y);
  xs = XmStringCreateSimple(str);
  count = 0;
  XtSetArg(args[count], XmNlabelString, xs);  count++;
  XtSetValues(button, args, count);
  XmStringFree(xs);
}

void
Resizer::stateChanged(VkComponent *, void *, void *callData)
{
  VkResizerReason reason = (VkResizerReason) callData;

  if (reason == VR_resized || reason == VR_moved) {
    setLabel();
  }
}

void
Resizer::button_activate(Widget, XtPointer clientData, XtPointer)
{
  Resizer *obj = (Resizer *) clientData;

  obj->buttonActivate();
}

VkComponent *
createResizer(char *name, Widget parent)
{
  return new Resizer(name, parent);
}
