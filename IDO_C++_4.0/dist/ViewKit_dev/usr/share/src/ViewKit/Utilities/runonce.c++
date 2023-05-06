////////////////////////////////////////////////////
// runonce: demo the VkRunOnce class
//
// This application will only allow a single instance
// to be run on any display. Running the application
// a second time will simply notify the running application.
// The application expects to take two command line arguments.
// The first indicates a string to be displayed, the second
// a foreground color.
//////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkRunOnce.h>
#include <Vk/VkNameList.h>
#include <Vk/VkSimpleWindow.h>
#include <Xm/Label.h>

// Define a top-level window class
class RunOnceWindow : public VkSimpleWindow {

     Widget _label;


  public:

    RunOnceWindow ( const char *name );
    ~RunOnceWindow();

    void update(VkComponent *comp, XtPointer, XtPointer);

    virtual const char* className();
};

RunOnceWindow::RunOnceWindow ( const char *name ) : VkSimpleWindow ( name ) 
{ 
    _label =  XmCreateLabel ( mainWindowWidget(), "hello", NULL, 0 );
    
    addView(_label);
} 


RunOnceWindow::~RunOnceWindow()
{
    // Empty
}

const char* RunOnceWindow::className() { return "RunOnceWindow"; }

void RunOnceWindow::update(VkComponent *comp, XtPointer, XtPointer)
{
    // Just retrieve the arguments, Use the first string as a new label
    // for the widget and the second as a color.

    VkRunOnce *obj = (VkRunOnce*) comp;

    if(obj->numArgs() > 0)
    {
	XmString xmstr = XmStringCreateLocalized( obj->arg(0));

	XtVaSetValues(_label, XmNlabelString, xmstr, NULL);
    }

    if(obj->numArgs() > 1)
    {
	XtVaSetValues(_label, 
		      XtVaTypedArg, 
		      XmNforeground, XmRString, obj->arg(1), strlen( obj->arg(1) ) + 1, NULL);
    }
}


// Main driver. Instantiate a VkApp and a top-level window, "show" 
// the window and then "run" the application. The VkRunOnce object
// prevents multiple instances of the application.

void main ( int argc, char **argv )
{
    VkApp        *app = new VkApp("Hello", &argc, argv);

    // Construct a VkNameList object to pass arguments
    // And load all left-over command line arguments

    VkNameList *args = new VkNameList();

    for(int i = 1; i < argc; i++)
	args->add(argv[i]);

    VkRunOnce *runOnce = new VkRunOnce(args);

    // Create a window
    
    RunOnceWindow  *win = new RunOnceWindow("hello");

    // Register a callback for running applications to recieve if
    // anyone attempts to run this application again.

    VkAddCallbackMethod(VkRunOnce::invokedCallback, runOnce, 
			win, RunOnceWindow::update, NULL);

    win->show();
    app->run();
}




