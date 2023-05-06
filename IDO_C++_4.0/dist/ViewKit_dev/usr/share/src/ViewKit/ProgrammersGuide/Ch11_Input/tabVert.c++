#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include <Vk/VkTabPanel.h>

class TabWindow: public VkSimpleWindow {
    
  protected:

    VkTabPanel *tp;
    static String _defaultResources[];

  public:
    
    TabWindow ( const char *name );
    ~TabWindow();
    virtual const char* className();
};


String TabWindow::_defaultResources[] =
{
    "*tabPanel*tabHeight:  55",
    "*tabPanel*one:        First",
    "*tabPanel*two:        Second",
    "*tabPanel*three:      Third",
    "*tabPanel*four:       Fourth",
    "*tabPanel*five:       Fifth",
    NULL
};

TabWindow::TabWindow ( const char *name ) : VkSimpleWindow ( name )
{
    setDefaultResources(mainWindowWidget(), _defaultResources);
    tp = new VkTabPanel("tabPanel", mainWindowWidget(), FALSE);

    tp->addTab("one", NULL);
    tp->addTab("two", NULL);
    tp->addTab("three", NULL);
    tp->addTab("four", NULL);
    tp->addTab("five", NULL);

    addView(tp);
}

TabWindow::~TabWindow()
{
    // Empty
}

const char * TabWindow::className()
{
    return "TabWindow";
}


void main ( int argc, char **argv )
{
    VkApp      *app = new VkApp("Tab", &argc, argv);
    TabWindow  *win = new TabWindow("tabWin");
    
    win->show();
    app->run();
}
