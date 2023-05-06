#include <stdio.h>
#include <Vk/VkScroll.h>
#include <Vk/VkComponent.h>
#include <Vk/VkResource.h>
#include <Xm/DrawingA.h>
#include <Xm/Form.h>

static int count;
static Arg args[10];

class Scroll : public VkComponent {
public:
  Scroll(const char *name, Widget parent);
  ~Scroll();

protected:
  void areaExpose();
  void scrollProc(VkComponent *comp, void *clientData, void *callData);

  static void area_expose(Widget w, XtPointer clientData, XtPointer callData);

  Widget area;
  VkScroll *scroll;
  GC gc;
};

Scroll::Scroll(const char *name, Widget parent)
: VkComponent(name)
{
  XGCValues gcv;

  count = 0;
  _baseWidget = XmCreateForm(parent, (char *) name, args, count);

  scroll = new VkScroll("scroll", _baseWidget);
  VkAddCallbackMethod(VkScroll::scrollCallback, scroll, this,
		      Scroll::scrollProc, NULL);
  scroll->setAnnotations();
  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetValues(scroll->baseWidget(), args, count);

  count = 0;
  XtSetArg(args[count], XmNheight, 1000);  count++;
  XtSetArg(args[count], XmNwidth, 1000);  count++;
  area = XmCreateDrawingArea(scroll->getClip(), "area", args, count);
  XtManageChild(area);
  XtAddCallback(area, XmNexposeCallback, Scroll::area_expose,
		(XtPointer) this);

  scroll->setChild(area);
  scroll->show();

  gcv.function = GXcopy;
  gcv.foreground = (Pixel) VkGetResource(_baseWidget, "selectColor",
					 "SelectColor", XmRPixel,
					 "red");
  gc = XtGetGC(_baseWidget, GCFunction | GCForeground, &gcv);

  installDestroyHandler();
}

Scroll::~Scroll()
{
  delete scroll;
}

void
Scroll::areaExpose()
{
  Dimension width, height;

  count = 0;
  XtSetArg(args[count], XmNwidth, &width);  count++;
  XtSetArg(args[count], XmNheight, &height);  count++;
  XtGetValues(area, args, count);
  XFillRectangle(XtDisplay(area), XtWindow(area), gc,
		 0, height/2, width, height/10); 
}

void
Scroll::scrollProc(VkComponent *, void *, void *callData)
{
  VkScrollCallback *cb = (VkScrollCallback *) callData;

  if (cb->reason == VS_troughPixmap) {
    XFillRectangle(XtDisplay(_baseWidget), cb->pixmap, gc,
		   0, cb->height/2, cb->width, cb->height/10);
  }
}

void
Scroll::area_expose(Widget, XtPointer clientData, XtPointer)
{
  Scroll *obj = (Scroll *) clientData;

  obj->areaExpose();
}

VkComponent *
createScroll(char *name, Widget parent)
{
  return new Scroll(name, parent);
}
