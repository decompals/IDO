#ifndef _C_LIST_
#define _C_LIST_

#include <Vk/VkComponent.h>

class Carea;

class Clist : public VkComponent {

public:

  Clist(const char *name, Widget parent);
  ~Clist();

  virtual const char* className();

  void setArea(Carea *area) { carea = area; }

protected:
  void displayComponents(Widget list);
  void listSelect(int which);

  static void list_select(Widget w, XtPointer clientData, XtPointer callData);

  Widget label, list;
  Carea *carea;
  VkComponent *lastComponent;
};

#endif
