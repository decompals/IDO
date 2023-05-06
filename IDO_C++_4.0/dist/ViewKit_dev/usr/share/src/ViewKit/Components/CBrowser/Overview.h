#ifndef _C_OVERVIEW_
#define _C_OVERVIEW_

#include <Vk/VkSimpleWindow.h>

class Celement;

class Coverview : public VkSimpleWindow {

public:

  Coverview(const char *name);
  ~Coverview();

protected:
  virtual Widget setUpInterface(Widget parent); 
  virtual void handleWmDeleteMessage();

  Widget grid;
  Celement **elements;
  int numElements;
};

#endif
