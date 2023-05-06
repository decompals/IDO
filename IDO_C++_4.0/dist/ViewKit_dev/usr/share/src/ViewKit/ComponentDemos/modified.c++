///////////////////////////////////////////////////////////////////
// Demo the VkModified component
/////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkModified.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/Text.h>


class SampleWindow : public VkSimpleWindow {

  public:

    SampleWindow(const char *name);
    ~SampleWindow();

  protected:

    VkModifiedAttachment *m1, *m2;

  private:

    static void activateCallback(Widget, 
				 XtPointer,
				 XtPointer);


};

SampleWindow::SampleWindow(const char *name) : VkSimpleWindow(name)
{
    Widget form, text, text2, label, label2;

    Arg args[20];
    int count;

    count = 0;
    form = XmCreateForm((*this), "form", args, count);

    count = 0;
    XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNtopOffset, 10);  count++;
    XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNleftOffset, 10);  count++;
    XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNrightOffset, 10);  count++;
    XtSetArg(args[count], XmNheight, 30);  count++;
    text = XmCreateText(form, "text", args, count);
    XtManageChild(text);
    XtAddCallback(text, XmNactivateCallback,
		  SampleWindow::activateCallback, (XtPointer) this);

    m1 = new VkModifiedAttachment();    
    m1->attach(text);
    m1->show();
    
    count = 0;
    XtSetArg(args[count], XmNtopAttachment, XmATTACH_WIDGET);  count++;
    XtSetArg(args[count], XmNtopWidget, text);  count++;
    XtSetArg(args[count], XmNtopOffset, 10);  count++;
    XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNleftOffset, 10);  count++;
    XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNrightOffset, 10);  count++;
    XtSetArg(args[count], XmNheight, 30);  count++;
    text2 = XmCreateText(form, "text2", args, count);
    XtManageChild(text2);
    
    count = 0;
    XtSetArg(args[count], XmNtopAttachment, XmATTACH_WIDGET);  count++;
    XtSetArg(args[count], XmNtopWidget, text2);  count++;
    XtSetArg(args[count], XmNtopOffset, 10);  count++;
    XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNleftOffset, 10);  count++;
    XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNrightOffset, 10);  count++;
    XtSetArg(args[count], XmNheight, 30);  count++;
    XtSetArg(args[count], XmNrecomputeSize, False);  count++;
    label = XmCreateLabelGadget(form, "label", args, count);
    XtManageChild(label);

    m2 = new VkModifiedAttachment();
    
    m2->attach(label);
    m2->show();
    
    count = 0;
    XtSetArg(args[count], XmNtopAttachment, XmATTACH_WIDGET);  count++;
    XtSetArg(args[count], XmNtopWidget, label);  count++;
    XtSetArg(args[count], XmNtopOffset, 10);  count++;
    XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNbottomOffset, 10);  count++;
    XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNleftOffset, 10);  count++;
    XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNrightOffset, 10);  count++;
    XtSetArg(args[count], XmNheight, 30);  count++;
    XtSetArg(args[count], XmNrecomputeSize, False);  count++;
    label2 = XmCreateLabelGadget(form, "label2", args, count);
    XtManageChild(label2);
    
    addView(form);
}

SampleWindow::~SampleWindow()
{
    delete m1;
    delete m2;
}

void SampleWindow::activateCallback(Widget w, XtPointer clientData, XtPointer)
{
    SampleWindow *obj = (SampleWindow *) clientData;
    char *str;

    str = XmTextGetString(w);
    obj->m2->setValue(str);
    XtFree(str);
}

void main(int argc, char **argv)
{
    VkApp *app = new VkApp("Modified", &argc, argv);
    SampleWindow *sw = new SampleWindow("modified");

    sw->show();
    app->run();
}
