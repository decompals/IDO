#include <stdio.h>
#include <Vk/VkApp.h>
#include <Vk/VkWindow.h>
#include <Vk/VkPrefDialog.h>
#include <Vk/VkPrefItem.h>
#include <Xm/PushB.h>

////////////////////////////////////////////////////////
// Create the Sample Preference dialog
/////////////////////////////////////////////////////////

class ExamplePref : public VkPrefDialog {
  
   protected:

      VkPrefList   *prefList;
      VkPrefGroup  *prefGroup;
      VkPrefText   *prefText  ;
      VkPrefText   *prefText2 ;
      VkPrefText   *prefText3 ;
      VkPrefToggle *prefToggle ;
      VkPrefOption *prefOption ;

   public:

     ExamplePref(const char *name) ;
     ~ExamplePref() ;
};

ExamplePref::ExamplePref(const char *name)   : VkPrefDialog(name) 
{
    // Create three text input fields. Init the contents of some of them

    prefText     = new VkPrefText("textPref1");
    prefText->setValue("one");

    prefText2    = new VkPrefText("textPref2");

    prefText3    = new VkPrefText("textPrefThree");
    prefText3->setValue("three");

    // Create a toggle button, set to true initially

    prefToggle   = new VkPrefToggle("togglePref");
    prefToggle->setValue(True);

    // Create an option menu with four items

    prefOption   = new VkPrefOption("optionPref", 4);

    prefOption->setLabel(0, "one");
    prefOption->setLabel(1, "two");
    prefOption->setLabel(2, "three");
    prefOption->setLabel(3, "four");

    // Now create a group to hold all the above items and add them

    prefGroup    = new VkPrefGroup("textGroupPrefs");

    prefGroup->addItem(prefText);
    prefGroup->addItem(prefText2);
    prefGroup->addItem(prefText3);
    prefGroup->addItem(prefToggle);
    prefGroup->addItem(prefOption);


    // Create a set of toggles

    VkPrefToggle *toggle1 = new VkPrefToggle("toggle1");
    VkPrefToggle *toggle2 = new VkPrefToggle("toggle2");
    VkPrefToggle *toggle3 = new VkPrefToggle("toggle3");

    // Create a Radio grou and add all the above toggles

    VkPrefGroup *toggleGroup = new VkPrefRadio("toggleGroup", True);

    toggleGroup->addItem(toggle1);
    toggleGroup->addItem(toggle2);
    toggleGroup->addItem(toggle3);

    // Create  a seocnd set of toggles and another radio group
    
    VkPrefToggle *t1a = new VkPrefToggle("toggle1");
    VkPrefToggle *t2a = new VkPrefToggle("toggle2");
    VkPrefToggle *t3a = new VkPrefToggle("toggle3");

    VkPrefGroup  *tga = new VkPrefRadio("toggleGroupa", False, True);
    
    tga->addItem(t1a);
    tga->addItem(t2a);
    tga->addItem(t3a);

    // And still another set of toggles, placed in a pref group

    VkPrefToggle *t1b = new VkPrefToggle("toggle1beforeEmpty");
    VkPrefEmpty  *t2b = new VkPrefEmpty();
    VkPrefToggle *t3b = new VkPrefToggle("toggle3afterEmpty");

    VkPrefGroup  *tgb = new VkPrefGroup("toggleGroupb", False, True);
    
    tgb->addItem(t1b);
    tgb->addItem(t2b);
    tgb->addItem(t3b);

    // Create a horizontal pref group and add the last two toggles

    VkPrefGroup *hg = new VkPrefGroup("horizGroup", True);
    
    hg->addItem(tga);
    hg->addItem(tgb);

    // Create a preference list to hold everything. Add the group, the
    // horizontal list, and the radio toggle group

    prefList     = new VkPrefList("prefs");
    prefList->addItem(prefGroup);
    prefList->addItem(toggleGroup);
    prefList->addItem(hg);

    // Install this as the dialog's top-level preference item

    setItem(prefList);
}

ExamplePref::~ExamplePref() 
{
    // Empty
}


////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////
// Fairly Generic stuff for the sample application
////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////
// A top level window class from which to post the dialog

class TestWindow : public VkWindow {

    private:

     static void postDialogCallback(Widget, XtPointer, XtPointer);

   protected:

      ExamplePref *_pref;
      void postDialog(Widget, XtPointer);

      void handlePrefs(VkComponent *, void *, void *);
      void reportChanges( VkPrefItem *, int, int );
 
   public:

     TestWindow(const char *name);
     ~TestWindow();

     virtual Widget setUpInterface(Widget parent);
};

// Constructor

TestWindow::TestWindow(const char *name) : VkWindow(name)
{
    // Create a pref dialog object

    _pref = new ExamplePref("testPref");

    // Install a member function to be called when the preferences are changed

    VkAddCallbackMethod(VkPrefDialog::prefCallback, _pref, this, &TestWindow::handlePrefs, (void *) NULL);
}


// Destructor

TestWindow::~TestWindow()
{
    delete _pref;  // Just free the pref dialog
}


Widget TestWindow::setUpInterface(Widget parent)
{
    // Simple UI that just provides a button to launch a pref dialog

    Widget button;

    button = XmCreatePushButton(parent, "test", NULL, 0);

    XtAddCallback(button, XmNactivateCallback,
		  &TestWindow::postDialogCallback,
		  (XtPointer) this);

    return button;
}


// Typical static member function/member function pair to interface to Motif callbacks

void TestWindow::postDialogCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    TestWindow *obj = (TestWindow*) clientData;
    
    obj->postDialog(w, callData);
}

void TestWindow::postDialog(Widget, XtPointer)
{
    // Display the preference dialog

    _pref->post("");  
}

//  Print the current status is the pref dialog

void TestWindow::reportChanges(VkPrefItem *item, int num, int indent)
{
  int each;
  
  for (each=0; each<indent; each++)
      printf(" ");

  if (item->changed()) 
      printf("Item %d changed\n", num);
  else
      printf("Item %d unchanged\n", num);

  if (item->type() == PI_option) 
  {
      for (each = 0; each < indent; each++)
	  printf(" ");
      
      printf("  (value = %d)\n", ((VkPrefOption *) item)->getValue());
  }
  
  if (item->isContainer()) 
  {
      VkPrefGroup *prefGroup = (VkPrefGroup *) item;

      for (each = 0; each < prefGroup->size(); each++) 
	  reportChanges(prefGroup->item(each), each, indent + 2);
  }
}

void TestWindow::handlePrefs(VkComponent * comp, void *, void *)
{
    VkPrefDialog *pd = (VkPrefDialog *) comp;

    // Call auxiliarry function because this needs to be recursive

    reportChanges(pd->item(), 0, 0);
}


// Main Driver

void main(int argc, char **argv)
{
    VkApp      app ("PrefTest", &argc, argv);
    TestWindow win ("prefTestWindow");

    win.show();
    app.run();
}







