// This is a sample driver program based on the ViewKit.
// This program instantiates a VkApp and creates a VkWindow subclass.
// to be used to test the DialogPoster class

#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkSubMenu.h>
#include "DialogPoster.h"


class SampleWindow: public VkWindow { 

  private:

        static void quitCallback(Widget, XtPointer, XtPointer); 

  protected:

        virtual Widget setUpInterface ( Widget ); 

  public:

    SampleWindow ( const char *name ) : VkWindow ( name ) { }
    ~SampleWindow ();
    virtual const char* className() { return "SampleWindow"; }
};


SampleWindow::~SampleWindow()
{


}

Widget SampleWindow::setUpInterface ( Widget parent ) 
{ 
    // Instantiate the class we are testing 

    DialogPoster* obj =  new DialogPoster("sample", parent);

    // Add a quit menu item 

    VkSubMenu *pane = addMenuPane("File");

    pane->addAction("Quit", &SampleWindow::quitCallback, (XtPointer) this);

    // Display the component

    obj->show();

    return obj->baseWidget();
}

void SampleWindow::quitCallback ( Widget, XtPointer clientData, XtPointer ) 
{
    SampleWindow *obj = (SampleWindow *) clientData;
    delete obj;
}


void main ( int argc, char **argv )
{
    VkApp        *sampleApp = new VkApp("DialogTest", &argc, argv);
    SampleWindow  *sampleWin = new SampleWindow("sample");
    SampleWindow  *sampleWin2 = new SampleWindow("sample");

    sampleWin->show();
    sampleWin2->show();
    sampleApp->run();
}




