//////////////////////////////////////////////////////////////
// Generic ViewKit application with a \`safe quit\' menu item
/////////////////////////////////////////////////////////////
#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkOutlineASB.h>
#include <Vk/VkSubMenu.h>


class MyWindow : public VkWindow {

   private:

	static void highlightCallback (Widget, XtPointer, XtPointer);
	static void unhighlightCallback (Widget, XtPointer, XtPointer);

   protected:

      VkOutlineASB *_outliner;

   public:

     MyWindow(const char *name);  
     int hilite_handle;
};

static char* highlight_strings [] = {
	"SubHeading 1",
	"SubHeading 5",
	"SubHeading 8",
	NULL
};

void MyWindow::highlightCallback (Widget, XtPointer clientData, XtPointer)
{
    MyWindow* mywindow = (MyWindow*) clientData;
    mywindow->_outliner->highlight (highlight_strings, mywindow->hilite_handle);
}

void MyWindow::unhighlightCallback (Widget, XtPointer clientData, XtPointer)
{
    MyWindow* mywindow = (MyWindow*) clientData;
    mywindow->_outliner->unhighlight (mywindow->hilite_handle);
}

MyWindow::MyWindow(const char *name) : VkWindow(name)
{
    VkSubMenu* pane = addMenuPane ("Highlight");
    pane->addAction ("highlight", &MyWindow::highlightCallback,
					(XtPointer)this);
    pane->addAction ("unhighlight", &MyWindow::unhighlightCallback,
					(XtPointer)this);
    
    addView(_outliner = new VkOutlineASB("outliner", (*this)));

    _outliner->add("Heading 1",   "SubHeading 1");
    _outliner->add("Heading 1",   "SubHeading 2");
    _outliner->add("Heading 1",   "SubHeading 3");
    _outliner->add("SubHeading 1", "Item 1");
    _outliner->add("Heading 2",   "SubHeading 5");
    _outliner->add("Heading 2",   "SubHeading 6");
    _outliner->add("Heading 2",   "SubHeading 7");
    _outliner->add("Heading 3",   "SubHeading 8");
    _outliner->add("Heading 3",   "SubHeading 9");
    _outliner->add("Heading 3",   "SubHeading A");

    XmFontList font;
    Pixel bg, fg;
    XtVaGetValues (	_outliner->listWidget(),
			XmNbackground, &bg,
			XmNforeground, &fg,
			XmNfontList,   &font,
			NULL);

    hilite_handle = _outliner->setHighlightAttributes (bg, 21, bg, font);
    _outliner->setAnnotation (hilite_handle, True);

    _outliner->displayAll();
}

void main ( int argc, char **argv )
{
    VkApp     *app   = new VkApp("Outline", &argc, argv);
    MyWindow  *win   = new MyWindow("outline");

    win->show();  // Display the window
    app->run();   // Run the application
}
