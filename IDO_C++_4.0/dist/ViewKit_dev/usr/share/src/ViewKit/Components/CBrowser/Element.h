#ifndef _C_ELEMENT_
#define _C_ELEMENT_

#include <Vk/VkComponent.h>

class Celement : public VkComponent {

public:

  Celement(const char *name, Widget parent, int componentIndex);
  ~Celement();

protected:

  Widget label, sep;
  VkComponent *component;
};

#endif
