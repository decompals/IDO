#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkOutline.h>

class OutlineWindow: public VkSimpleWindow {
    
  protected:

    VkOutline *outline;
    static char* keys[];
    static char* display_names[];

  public:
    
    OutlineWindow ( const char *name );
    ~OutlineWindow();
    virtual const char* className();
};


char* OutlineWindow::keys[] = {
    "first1",
    "second1",
    "third1",
    NULL
};

char* OutlineWindow::display_names[] = {
    "Top level",
    "Second level",
    "Third level",
    NULL
};

OutlineWindow::OutlineWindow ( const char *name ) : VkSimpleWindow ( name )
{ 
    outline = new VkOutline("outline", mainWindowWidget());
    
    outline->createPath(display_names, keys);
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
