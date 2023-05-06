#include <stdio.h>
#include <libc.h>
#include <malloc.h>
#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkTabPanel.h>
#include <Vk/VkResource.h>

#include <Xm/Form.h>
#include <Xm/LabelG.h>
#include <Xm/Text.h>

#define xlogo11_width 11
#define xlogo11_height 11
static char xlogo11_bits[] = {
 0x0f, 0x04, 0x0f, 0x02, 0x1e, 0x01, 0x3c, 0x01, 0xb8, 0x00, 0x58, 0x00,
 0xe8, 0x00, 0xe4, 0x01, 0xc4, 0x03, 0xc2, 0x03, 0x81, 0x07 };

#define star_width 16
#define star_height 16
#define star_x_hot 7
#define star_y_hot 7
static char star_bits[] = {
   0x00, 0x00, 0x80, 0x00, 0x80, 0x00, 0x88, 0x08, 0x90, 0x04, 0xa0, 0x02,
   0x40, 0x01, 0x3e, 0x3e, 0x40, 0x01, 0xa0, 0x02, 0x90, 0x04, 0x88, 0x08,
   0x80, 0x00, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00};

static Arg args[10];
static int count;

class SampleWindow : public VkSimpleWindow {
public:
  SampleWindow(const char *name);
  ~SampleWindow();

  virtual Widget setUpInterface(Widget parent);

protected:
  void tabSelect(VkComponent *comp, void *clientData, void *callData);
  void tabPopup(VkComponent *comp, void *clientData, void *callData);

  static void text_stub(Widget w, XtPointer clientData, XtPointer callData);

  VkTabPanel *tpanel;
  Widget label1, label2, header, text;
};

SampleWindow::SampleWindow(const char *name)
: VkSimpleWindow(name)
{
  header = NULL;
}

SampleWindow::~SampleWindow()
{
}

Widget
SampleWindow::setUpInterface(Widget parent)
{ 
  Widget form;
  Pixmap p;

  count = 0;
  XtSetArg(args[count], XmNwidth, 400);  count++;
  form = XmCreateForm(parent, "form", args, count);

  count = 0;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  text = XmCreateText(form, "text", args, count);
  XtManageChild(text);
  XtAddCallback(text, XmNactivateCallback, SampleWindow::text_stub,
		(XtPointer) this);

  tpanel = new VkTabPanel("tabPanel", form);
  count = 0;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNbottomWidget, text);  count++;
  XtSetValues(tpanel->baseWidget(), args, count);
  tpanel->show();
  VkAddCallbackMethod(VkTabPanel::tabSelectCallback, tpanel, this,
		      SampleWindow::tabSelect, NULL);
  VkAddCallbackMethod(VkTabPanel::tabPopupCallback, tpanel, this,
		      SampleWindow::tabPopup, NULL);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  label1 = XmCreateLabelGadget(tpanel->area1(), "Nifty Tabs:", args, count);
  XtManageChild(label1);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  label2 = XmCreateLabelGadget(tpanel->area2(), "(Right Edge)", args, count);
  XtManageChild(label2);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNbottomWidget, tpanel->baseWidget());  count++;
  XtSetArg(args[count], XmNrecomputeSize, False);  count++;
  header = XmCreateLabelGadget(form, "header", args, count);
  XtManageChild(header);

  tpanel->addTab("one", NULL);
  tpanel->addTab("two", NULL);
  tpanel->addTab("three", NULL);
  tpanel->addTab("four", NULL);
  tpanel->addTab("five", NULL);
  tpanel->addTab("six", NULL);
  tpanel->addTab("seven", NULL);
  tpanel->addTab("eight", NULL);
  tpanel->addTab("nine", NULL);
  tpanel->addTab("ten", NULL);

  p = XCreateBitmapFromData(XtDisplay(_baseWidget),
			    XRootWindowOfScreen(XtScreen(_baseWidget)),
			    xlogo11_bits,
			    xlogo11_width, xlogo11_height);
  tpanel->setTabPixmap("four", p);

  tpanel->selectTab("three");

  XtManageChild(form);
  return form;
}

void
SampleWindow::tabSelect(VkComponent *, void *, void *callData)
{
  VkTabCallbackStruct *cb = (VkTabCallbackStruct *) callData;
  XmString xs;

  if (header) {
    if (cb->tabIndex == -1) {
      xs = XmStringCreateSimple("");
    } else {
      xs = XmStringCreateSimple(cb->label);
    }
    count = 0;
    XtSetArg(args[count], XmNlabelString, xs);  count++;
    XtSetValues(header, args, count);
    XmStringFree(xs);
  }
}

void
SampleWindow::tabPopup(VkComponent *, void *, void *callData)
{
  VkTabCallbackStruct *cb = (VkTabCallbackStruct *) callData;

  tpanel->removeTab(cb->tabIndex);
}

void
SampleWindow::text_stub(Widget, XtPointer clientData, XtPointer)
{
  SampleWindow *obj = (SampleWindow *) clientData;
  char *str;

  str = XmTextGetString(obj->text);
  obj->tpanel->addTab(str, NULL, True);
  obj->tpanel->selectTab(str);
  XmTextSetString(obj->text, "");
  XtFree(str);
}

/**********************************************************************/

class SampleWindow2 : public VkSimpleWindow {
public:
  SampleWindow2(const char *name);
  ~SampleWindow2();

  virtual Widget setUpInterface(Widget parent);

protected:
  void tabSelect(VkComponent *comp, void *clientData, void *callData);
  void tabPopup(VkComponent *comp, void *clientData, void *callData);

  static void text_stub(Widget w, XtPointer clientData, XtPointer callData);

  VkTabPanel *tpanel;
  Widget label1, label2, header, text;
};

SampleWindow2::SampleWindow2(const char *name)
: VkSimpleWindow(name)
{
  header = NULL;
}

SampleWindow2::~SampleWindow2()
{
}

Widget
SampleWindow2::setUpInterface(Widget parent)
{ 
  Widget form;
  Pixmap p;

  count = 0;
  XtSetArg(args[count], XmNheight, 300);  count++;
  form = XmCreateForm(parent, "form", args, count);

  count = 0;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  text = XmCreateText(form, "text", args, count);
  XtManageChild(text);
  XtAddCallback(text, XmNactivateCallback, SampleWindow2::text_stub,
		(XtPointer) this);

  tpanel = new VkTabPanel("tabPanel", form, False);
  count = 0;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNbottomWidget, text);  count++;
  XtSetValues(tpanel->baseWidget(), args, count);
  tpanel->show();
  VkAddCallbackMethod(VkTabPanel::tabSelectCallback, tpanel, this,
		      SampleWindow2::tabSelect, NULL);
  VkAddCallbackMethod(VkTabPanel::tabPopupCallback, tpanel, this,
		      SampleWindow2::tabPopup, NULL);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  label1 = XmCreateLabelGadget(tpanel->area1(), "A", args, count);
  XtManageChild(label1);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
  label2 = XmCreateLabelGadget(tpanel->area2(), "Z", args, count);
  XtManageChild(label2);

  count = 0;
  XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNleftAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNleftWidget, tpanel->baseWidget());  count++;
  XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
  XtSetArg(args[count], XmNbottomAttachment, XmATTACH_WIDGET);  count++;
  XtSetArg(args[count], XmNbottomWidget, text);  count++;
  XtSetArg(args[count], XmNrecomputeSize, False);  count++;
  header = XmCreateLabelGadget(form, "header", args, count);
  XtManageChild(header);

  tpanel->addTab("1", NULL);
  tpanel->addTab("2", NULL);
  tpanel->addTab("3", NULL);
  tpanel->addTab("4", NULL);
  tpanel->addTab("5", NULL);
  tpanel->addTab("6", NULL);
  tpanel->addTab("7", NULL);
  tpanel->addTab("8", NULL);
  tpanel->addTab("9", NULL);
  tpanel->addTab("10", NULL);

  p = XCreatePixmapFromBitmapData(XtDisplay(_baseWidget),
				  XRootWindowOfScreen(XtScreen(_baseWidget)),
				  star_bits,
				  star_width, star_height,
// Easiest way to allocate colors by name
				  (Pixel) VkGetResource(_baseWidget,
							"nonExistent",
							"nonExistent",
							XmRPixel,
							sizeof(Pixel),
							"blue"),
				  (Pixel) VkGetResource(_baseWidget,
							"nonExistent",
							"nonExistent",
							XmRPixel,
							sizeof(Pixel),
							"red"),
				  XDefaultDepthOfScreen(XtScreen(_baseWidget)));
  tpanel->setTabPixmap("4", p);

  tpanel->selectTab("3");

  XtManageChild(form);
  return form;
}

void
SampleWindow2::tabSelect(VkComponent *, void *, void *callData)
{
  VkTabCallbackStruct *cb = (VkTabCallbackStruct *) callData;
  XmString xs;

  if (header) {
    if (cb->tabIndex == -1) {
      xs = XmStringCreateSimple("");
    } else {
      xs = XmStringCreateSimple(cb->label);
    }
    count = 0;
    XtSetArg(args[count], XmNlabelString, xs);  count++;
    XtSetValues(header, args, count);
    XmStringFree(xs);
  }
}

void
SampleWindow2::tabPopup(VkComponent *, void *, void *callData)
{
  VkTabCallbackStruct *cb = (VkTabCallbackStruct *) callData;

  tpanel->removeTab(cb->tabIndex);
}

void
SampleWindow2::text_stub(Widget, XtPointer clientData, XtPointer)
{
  SampleWindow2 *obj = (SampleWindow2 *) clientData;
  char *str;

  str = XmTextGetString(obj->text);
  obj->tpanel->addTab(str, NULL, True);
  obj->tpanel->selectTab(str);
  XmTextSetString(obj->text, "");
  XtFree(str);
}

/**********************************************************************/

void
main(int argc, char **argv)
{
  VkApp *app = new VkApp("Tab", &argc, argv);

  SampleWindow2 *sw2 = new SampleWindow2("tab2");
  sw2->show();

  SampleWindow *sw = new SampleWindow("tab");
  sw->show();

  app->run();
}
