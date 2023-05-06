/////////////////////////////////////////////////////////////
// main.c++ main driver for Rolodex program
/////////////////////////////////////////////////////////////

#include <Vk/VkApp.h>
#include <Vk/VkSimpleWindow.h>
#include "Rolodex.h"

// Simple class that just supports a top-level window in
// which to place the Rolodex user interface component

class RolodexWindow: public VkSimpleWindow {

  public:

  RolodexWindow ( const char *name ) : VkSimpleWindow ( name ) 
  {
      addView(new Rolodex ( (*this), "rolodex" ));
  }

  virtual const char* className();
};

const char* RolodexWindow::className()
{ 
  return ("RolodexWindow"); 
}


// Main just instantiates a VkApp and a VkMainWindow subclass

void main ( int argc, char **argv )
{
    VkApp          *app = new VkApp("Rolodex", &argc, argv);
    RolodexWindow  *win = new RolodexWindow("rolodex");

    win->show();
    app->run();
}

