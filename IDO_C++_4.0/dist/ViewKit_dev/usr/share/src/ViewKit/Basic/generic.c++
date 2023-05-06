/////////////////////////////////////////////////////////////////////
// Generic. Simplest possible Vk application
////////////////////////////////////////////////////////////////////

// Include the class headers used in this example

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>

void main ( int argc, char **argv )
{
    // instantiate an application object

    VkApp           *app = new VkApp("Generic", &argc, argv);

    // instantiate a top-level window object

    VkSimpleWindow  *win = new VkSimpleWindow("generic");

    // make the window visible

    win->show();

    // Start handling events

    app->run();
}

