#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkOutline.h>

class OutlineWindow: public VkSimpleWindow {
    
  protected:

    VkOutline *outline;

  public:
    
    OutlineWindow ( const char *name );
    ~OutlineWindow();
    virtual const char* className();
};


OutlineWindow::OutlineWindow ( const char *name ) : VkSimpleWindow ( name )
{ 
    outline = new VkOutline("outline", mainWindowWidget());
    
    outline->add("Heading 1", "Subheading 1A");
    outline->add("Heading 1", "Subheading 1B");
    outline->add("Heading 1", "Subheading 1C");
    outline->add("Heading 2", "Subheading 2A");
    outline->add("Heading 2", "Subheading 2B");
    outline->add("Subheading 2B", "Item 2B/i)");
    outline->add("Subheading 2B", "Item 2B/ii)");
    outline->add("Item 2B/ii)", "Subitem 2B/ii/a)");
    outline->add("Item 2B/ii)", "Subitem 2B/ii/b)");
    outline->add("Subheading 2B", "Item 2B/iii)");
    outline->add("Heading 2", "Subheading 2C");
    outline->add("Heading 3", "Subheading 3A");
    outline->add("Heading 3", "Subheading 3B");
    outline->add("Heading 3", "Subheading 3C");
    outline->displayAll();

    addView(outline);
}

OutlineWindow::~OutlineWindow()
{
    // Empty
}

const char * OutlineWindow::className()
{
    return "OutlineWindow";
}


void main ( int argc, char **argv )
{
    VkApp          *app = new VkApp("Tab", &argc, argv);
    OutlineWindow  *win = new OutlineWindow("outlineWin");
    
    win->show();
    app->run();
}
