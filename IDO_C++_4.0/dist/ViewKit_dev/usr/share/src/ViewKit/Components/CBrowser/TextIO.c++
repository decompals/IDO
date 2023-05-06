#include <stdio.h>
#include <Vk/VkTextIO.h>

VkComponent *
createTextIO(char *name, Widget parent)
{
  return new VkTextIO(name, parent, "prompt> ");
}
