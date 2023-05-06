/////////////////////////////////////////////////////////////////
// Demo how to create a custom dialog
////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkGenericDialog.h>
#include <Xm/Scale.h>
#include <Xm/PushB.h>

///////////////////////////////
// Define a new dialog class
///////////////////////////////


#include <Vk/VkGenericDialog.h>

class CustomDialog : public VkGenericDialog {

  protected:

    virtual Widget createDialog(Widget parent);

  public:

    CustomDialog(const char *name) : VkGenericDialog(name) { };
    virtual ~CustomDialog();
};


// Define what should be created when a new dialog is needed

Widget CustomDialog::createDialog(Widget parent)
{
    // Call superclass to create dialog template

    Widget base =  VkGenericDialog::createDialog(parent);

    // Add a child of our own

    Widget scale = XmCreateScale(base, "scale", NULL, 0);
    XtManageChild(scale);

    return base;
}

CustomDialog::~CustomDialog()
{
    // Empty
}



// Now test the dialog

class MyWindow: public VkSimpleWindow {

  protected:

    static void postCallback(Widget, XtPointer, XtPointer);

    virtual Widget setUpInterface ( Widget parent )
    {
	Widget button =  XmCreatePushButton ( parent, "Push Me", NULL, 0 );

	XtAddCallback(button, XmNactivateCallback, 
		      &MyWindow::postCallback, 
		      (XtPointer) this);

	return button;	
    }

  public:

    MyWindow ( const char *name ) : VkSimpleWindow ( name ) { }
    virtual const char* className() { return "MyWindow"; }
};

void MyWindow::postCallback(Widget, XtPointer clientData, XtPointer)
{
    CustomDialog *custom = NULL;

    if(!custom)
	custom = new CustomDialog("custom");

    custom->post();
}

void main ( int argc, char **argv )
{
    VkApp     *app  = new VkApp("DialogDemoApp", &argc, argv);
    MyWindow  *win  = new MyWindow("DialogDemo");

    win->show();
    app->run();
}

