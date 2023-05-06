#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkModified.h>
#include <Xm/Form.h>
#include <Xm/Text.h>

class ModifiedWindow: public VkSimpleWindow {
    
  public:
    
    ModifiedWindow ( const char *name );
    ~ModifiedWindow();
    virtual const char* className();

  protected:

    Widget text;
    VkModifiedAttachment *mod;
    static void text_activate(Widget, XtPointer, XtPointer);
};


ModifiedWindow::ModifiedWindow ( const char *name ) : VkSimpleWindow ( name )
{
    int count;
    Arg args[10];
    
    count = 0;
    XtSetArg(args[count], XmNeditMode, XmSINGLE_LINE_EDIT);  count++;
    text = XmCreateText(mainWindowWidget(), "text", args, count);
    XtAddCallback(text, XmNactivateCallback,
                  (XtCallbackProc) &ModifiedWindow::text_activate, NULL);
    
    addView(text);
    
    mod = new VkModifiedAttachment();
    mod->attach(text);
    mod->show();
    
    mod->setValue("one");
    mod->setValue("two");
}

ModifiedWindow::~ModifiedWindow()
{
    // Empty
}

const char * ModifiedWindow::className()
{
    return "ModifiedWindow";
}

void ModifiedWindow::text_activate(Widget w, XtPointer, XtPointer)
{
    XmTextSetInsertionPosition(w, 0);
}

void main ( int argc, char **argv )
{
    VkApp           *app = new VkApp("Modified", &argc, argv);
    ModifiedWindow  *win = new ModifiedWindow("modifiedWin");
    
    win->show();
    app->run();
}
