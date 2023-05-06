#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkTickMarks.h>
#include <Xm/Form.h>
#include <Xm/Scale.h>

class TickWindow: public VkSimpleWindow {
    
  protected:

    VkTickMarks *tick1, *tick2;
    Widget form, scale1, scale2;

  public:
    
    TickWindow ( const char *name );
    ~TickWindow();
    virtual const char* className();
};


TickWindow::TickWindow ( const char *name ) : VkSimpleWindow ( name )
{
    int count;
    Arg args[10];

    count = 0;
    XtSetArg(args[count], XmNwidth, 200);  count++;
    XtSetArg(args[count], XmNheight, 200);  count++;
    form = XmCreateForm(mainWindowWidget(), "tickForm", args, count);
  
    count = 0;
    XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
    scale1 = XmCreateScale(form, "scale1", args, count);
    XtManageChild(scale1);
  
    tick1 = new VkTickMarks("tick1", form);
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
    scale2 = XmCreateScale(form, "scale2", args, count);
    XtManageChild(scale2);
  
    tick2 = new VkTickMarks("tick2", form, False);
    tick2->setScale(50, 100, 5, 0);
    count = 0;
    XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNleftAttachment, XmATTACH_WIDGET);  count++;
    XtSetArg(args[count], XmNleftWidget, scale2);  count++;
    XtSetArg(args[count], XmNbottomAttachment, XmATTACH_FORM);  count++;
    XtSetValues(tick2->baseWidget(), args, count);
    tick2->show();

    addView(form);
}

TickWindow::~TickWindow()
{
    // Empty
}

const char * TickWindow::className()
{
    return "TickWindow";
}


void main ( int argc, char **argv )
{
    VkApp       *app = new VkApp("Ticks", &argc, argv);
    TickWindow  *win = new TickWindow("tickWin");
    
    win->show();
    app->run();
}
