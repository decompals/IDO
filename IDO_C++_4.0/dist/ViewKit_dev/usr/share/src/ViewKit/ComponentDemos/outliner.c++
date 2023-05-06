//////////////////////////////////////////////////////////////
// Generic ViewKit application with a \`safe quit\' menu item
/////////////////////////////////////////////////////////////
#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkOutline.h>


class MyWindow : public VkWindow {

   protected:

      VkOutline *_outliner;

   public:

     MyWindow(const char *name);  
};

MyWindow::MyWindow(const char *name) : VkWindow(name)
{
    addView(_outliner = new VkOutline("outliner", (*this)));

    _outliner->add("Heading 1",   "SubHeading 1");
    _outliner->add("Heading 1",   "SubHeading 2");
    _outliner->add("Heading 1",   "SubHeading 3");
    _outliner->add("SubHeading 1", "Item 1");

    _outliner->displayAll();
}

void main ( int argc, char **argv )
{
    VkApp     *app   = new VkApp("Outline", &argc, argv);
    MyWindow  *win   = new MyWindow("outline");

    win->show();  // Display the window
    app->run();   // Run the application
}
