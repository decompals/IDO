////////////////////////////////////////////////////
// runonce: demo the VkRunOnce2 class
//
// This application will only allow a single instance
// to be run on any display. Running the application
// a second time will simply notify the running application.
// The application expects to take two command line arguments.
// The first indicates a string to be displayed, the second
// a foreground color.
//////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkRunOnce2.h>
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
    VkRunOnce2 *obj = (VkRunOnce2*) comp;


}


// Main driver. Instantiate a VkApp and a top-level window, "show" 
// the window and then "run" the application. The VkRunOnce2 object
// prevents multiple instances of the application.

void main ( int argc, char **argv )
{
    VkRunOnce2 *ro = new VkRunOnce2(TRUE);

    ro->notifyOthers(argv, argc);
    
    VkApp        *app = new VkApp("Hello", &argc, argv);

    ro->takeCharge();

    // Create a window
    
    RunOnceWindow  *win = new RunOnceWindow("hello");

    // Register a callback for running applications to recieve if
    // anyone attempts to run this application again.

    VkAddCallbackMethod(VkRunOnce2::invokedCallback, ro, 
			win, RunOnceWindow::update, NULL);

    win->show();
    app->run();
}




