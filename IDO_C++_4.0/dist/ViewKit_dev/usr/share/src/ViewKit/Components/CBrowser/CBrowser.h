#ifndef _C_BROWSER_
#define _C_BROWSER_

#include <Vk/VkWindow.h>

class VkMenu;
class Clist;
class Carea;
class Coverview;

class Cbrowser : public VkWindow {

public:

  Cbrowser(const char *docName);
  ~Cbrowser();

  virtual const char* className();

protected:
  virtual Widget setUpInterface(Widget parent);
  virtual void handleWmDeleteMessage();
  void fileMenu(VkMenu *parent);
  void showOverview();

  static void show_overview(Widget w, XtPointer clientData,
			    XtPointer callData);
  static void quit_menu(Widget w, XtPointer clientData, XtPointer callData);

  Clist *clist;
  Carea *carea;
  Coverview *coverview;
};

#endif
