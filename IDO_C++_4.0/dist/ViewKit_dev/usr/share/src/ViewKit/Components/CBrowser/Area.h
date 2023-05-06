#ifndef _C_AREA_
#define _C_AREA_

#include <Vk/VkComponent.h>

class Carea : public VkComponent {

public:

  Carea(const char *name, Widget parent);
  ~Carea();

  virtual const char* className();

  Widget getDisplayWidget() { return display; }
  void setLabel(char *str);

protected:

  Widget label, frame, display;
};

#endif
