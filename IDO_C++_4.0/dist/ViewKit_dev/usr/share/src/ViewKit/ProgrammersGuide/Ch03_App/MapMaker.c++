/////////////////////////////////////////////////////////////////
// Simple example of providing inof for the Product Info dialog
/////////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkMenu.h>
#include <stream.h>
#include <Xm/Label.h>
#include <stdlib.h>

class MyWindow: public VkWindow {

  private:

    static void quitCallback( Widget,  XtPointer , XtPointer);

    void quit();

    static VkMenuDesc appMenuPane[];
    static VkMenuDesc mainMenuPane[];

  public:
 
    MyWindow( const char *name);
    ~MyWindow();

    virtual const char* className();
};

MyWindow::MyWindow( const char *name) : VkWindow( name)
{
    Widget label =  XmCreateLabel(mainWindowWidget(), "a menu", NULL, 0);
    
    setMenuBar(mainMenuPane);

    addView(label);
}

MyWindow::~MyWindow()
{
    // Empty
}

const char* MyWindow::className() 
{
    return "MyWindow";
}


// The menu bar is essentially a set of cascading menu panes, so the
// top level of the menu tree is always defined as a list of submenus

VkMenuDesc  MyWindow::mainMenuPane[] = {
  { SUBMENU, "Application",  NULL, MyWindow::appMenuPane},
  { END}
};

VkMenuDesc MyWindow::appMenuPane[] = {
  { ACTION,   "Quit",    &MyWindow::quitCallback},
  { END},
};

void MyWindow::quitCallback ( Widget, XtPointer, XtPointer )
{
    theApplication->quitYourself();
}


void main(int argc, char **argv)
{
  VkApp     *myApp    = new VkApp("MapMaker",  &argc,  argv);
  MyWindow  *menuWin  = new MyWindow("MenuWindow");
  
  myApp->setVersionString("MapMaker 2.1");

  menuWin->show();
  myApp->run();
}

