///////////////////////////////////////////////////////////////////////////
// nestBusy.c++  A simple example of nested calls to busy().
//////////////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Xm/PushB.h>

// Declare a top-level window class

class ReportWindow: public VkSimpleWindow {

  public:

    ReportWindow ( const char *name );
   ~ReportWindow();
    virtual const char* className();
    void report();
    void sort();

  protected:

    static void reportCallback(Widget, XtPointer, XtPointer);

  private:

    static String _defaultResources[];
};

String ReportWindow::_defaultResources[] = {
  "*sortDialogMsg:    Sorting records...",
  "*reportDialogMsg:  Generating report...",
  NULL
};

// Constructor creates a pushbutton to launch an example task,
// and installs it as a view in the top-level window

ReportWindow::ReportWindow(const char *name) : VkSimpleWindow ( name )
{
    setDefaultResources(theApplication->baseWidget(), _defaultResources);

    Widget button =  XmCreatePushButton ( mainWindowWidget(), "Busy", NULL, 0 );
    
    XtAddCallback(button, XmNactivateCallback, 
		  &ReportWindow::reportCallback, 
		  (XtPointer) this);

    addView(button);
}

ReportWindow::~ReportWindow()
{
    // Empty
}

const char* ReportWindow::className() 
{ 
    return "ReportWindow"; 
}

void ReportWindow::sort()
{
    int i, j;

    theApplication->busy("sortDialogMsg");

    for ( i=0; i<=10000; i++ )
        for ( j=0; j<=10000; j++);

    theApplication->notBusy();    
}


void ReportWindow::report()
{
    int i, j;

    theApplication->busy("reportDialogMsg");

    for ( i=0; i<=10000; i++ )
        for ( j=0; j<=10000; j++);

    sort();

    for ( i=0; i<=10000; i++ )
        for ( j=0; j<=10000; j++);

    theApplication->notBusy();    
}

void ReportWindow::reportCallback(Widget, XtPointer clientData, XtPointer)
{
    ReportWindow *obj = (ReportWindow *) clientData;
    obj->report();
}

// Main driver

void main ( int argc, char **argv )
{
    VkApp *app = new VkApp("BusyApp", &argc, argv);
    ReportWindow *reportWin = new ReportWindow("Report");

    reportWin->show();
    app->run();
}

