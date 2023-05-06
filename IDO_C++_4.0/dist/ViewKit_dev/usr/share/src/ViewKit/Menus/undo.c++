////////////////////////////////////////////////////////////////
// Simple example to exercise Vk undo facilities
///////////////////////////////////////////////////////////////
#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkMenu.h>
#include <Vk/VkMenuItem.h>
#include <Vk/VkSubMenu.h>
#include <stream.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

class MyWindow: public VkWindow {
  
  private:
  
    static void pushCallback( Widget,  XtPointer, XtPointer);
    static void undoPushCallback( Widget,  XtPointer, XtPointer);
  
    static void oneCallback( Widget,  XtPointer , XtPointer);
    static void twoCallback( Widget,  XtPointer , XtPointer);
    static void threeCallback( Widget,  XtPointer , XtPointer);
  
    static void undoOneCallback( Widget,  XtPointer , XtPointer);
    static void undoTwoCallback( Widget,  XtPointer , XtPointer);
    static void undoThreeCallback( Widget,  XtPointer , XtPointer);
  
    static void quitCallback( Widget,  XtPointer , XtPointer);
  
    void quit();
    void one();
    void two();
    void three();
    void undoOne();
  
    void undoTwo();
    void undoThree();
  
    static VkMenuDesc appMenuPane[];
    static VkMenuDesc mainMenuPane[];
  
  public:

    MyWindow( const char *name);
    ~MyWindow( );
    virtual const char* className();
};

MyWindow::MyWindow( const char *name) : VkWindow( name) 
{
    Widget rc =  XmCreateRowColumn(mainWindowWidget(), "rc", NULL, 0);
    Widget label =  XmCreateLabel(rc, "an undo test", NULL, 0);
    Widget pb =  XmCreatePushButton(rc, "push", NULL, 0);

    XtAddCallback(pb, XmNactivateCallback, &MyWindow::pushCallback, (XtPointer) this);
    XtManageChild(label);
    XtManageChild(pb);
    
    setMenuBar(mainMenuPane);
    
    VkSubMenu *	editMenuPane = addMenuPane("Edit");
    
    editMenuPane->add(theUndoManager);
    
    addView(rc);
}

MyWindow::~MyWindow()
{

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
  { ACTION,   "Command One",     &MyWindow::oneCallback, NULL, NULL, &MyWindow::undoOneCallback },
  { ACTION,   "Command Two",     &MyWindow::twoCallback, NULL, NULL, &MyWindow::undoTwoCallback },
  { ACTION,   "Command Three",   &MyWindow::threeCallback, NULL, NULL, &MyWindow::undoThreeCallback },
  { SEPARATOR },
  { CONFIRMFIRSTACTION,   "Quit",    &MyWindow::quitCallback},
  { END},
};


void MyWindow::one()
{
    cout << "Command One executed" <<  "\n" << flush;
}

void MyWindow::two()
{
    cout << "Command Two executed" <<  "\n" << flush;
}

void MyWindow::three()
{
    cout << "Command Three executed" <<  "\n" << flush;
}

void MyWindow::undoOne()
{
    cout << "Undoing Command One" <<  "\n" << flush;
}

void MyWindow::undoTwo()
{
    cout << "UNdoing Command Two" <<  "\n" << flush;
}

void MyWindow::undoThree()
{
    cout << "Undoing Command Three" <<  "\n" << flush;
}

void MyWindow::oneCallback( Widget,  XtPointer clientData, XtPointer)
{
    MyWindow *obj = (MyWindow *) clientData;
    obj->one();
}
void MyWindow::twoCallback( Widget,  XtPointer clientData, XtPointer)
{
    MyWindow *obj = (MyWindow *) clientData;
    obj->two();
}
void MyWindow::threeCallback( Widget,  XtPointer clientData, XtPointer)
{
    MyWindow *obj = (MyWindow *) clientData;
    obj->three();
}

void MyWindow::undoOneCallback( Widget,  XtPointer clientData, XtPointer)
{
    MyWindow *obj = (MyWindow *) clientData;
    obj->undoOne();
}
void MyWindow::undoTwoCallback( Widget,  XtPointer clientData, XtPointer)
{
    MyWindow *obj = (MyWindow *) clientData;
    obj->undoTwo();
}
void MyWindow::undoThreeCallback( Widget,  XtPointer clientData, XtPointer)
{
    MyWindow *obj = (MyWindow *) clientData;
    obj->undoThree();
}

void MyWindow::quitCallback ( Widget, XtPointer clientData, XtPointer )
{
    MyWindow *obj = (MyWindow*) clientData;
    delete obj;
}


void MyWindow::pushCallback( Widget,  XtPointer clientData, XtPointer)
{
    cout << "doing a push command\n" << flush;

    theUndoManager->add("Push", &MyWindow::undoPushCallback, (XtPointer) clientData);
}


void MyWindow::undoPushCallback( Widget,  XtPointer clientData, XtPointer)
{
    cout << "undoing the push command\n" << flush;
}



main(int argc, char **argv)
{
  VkApp     *app    = new VkApp("Menudemo",  &argc,  argv);
  MyWindow  *win  = new MyWindow("MenuWindow");

  win->show();
  app->run();
}

