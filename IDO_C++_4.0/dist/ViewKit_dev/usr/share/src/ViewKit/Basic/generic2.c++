//////////////////////////////////////////////////////////////
// Generic ViewKit application with a "safe quit" menu item
/////////////////////////////////////////////////////////////

// Include headers for needed classes

#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkSubMenu.h>

// Provide the function to be executed to really quit the application
// THis must be an Xt-style callback

void quitCallback(Widget, XtPointer, XtPointer)
{   
    theApplication->terminate(0); // Clean shut down function, provides an
                                  // opportunity to clean up first
}

void main ( int argc, char **argv )
{
    // Instantiate an application object

    VkApp      *app   = new VkApp("Generic", &argc, argv);

    // Create a top level window object

    VkWindow   *win   = new VkWindow("generic");

    // Add a menu pane named "Application" to the window's menu

    VkSubMenu  *pane  = win->addMenuPane("Application");
    
    // Add an item to the Application pane. Action requires user confirmation first

    pane->addConfirmFirstAction("Quit", quitCallback);

    win->show();  // Display the window

    app->run();   // Run the application
}
