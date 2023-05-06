#include <math.h>
#include <Vk/VkOutline.h>

VkComponent *
createOutline(char *name, Widget parent)
{
  VkOutline *outline;

  outline = new VkOutline(name, parent);
  outline->add("root", "a");
  outline->add("root", "b");
  outline->add("b", "c");
  outline->add("b", "d");
  outline->displayAll();

  return outline;
}
