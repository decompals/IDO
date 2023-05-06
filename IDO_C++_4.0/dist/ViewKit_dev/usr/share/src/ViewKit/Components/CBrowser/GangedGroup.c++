#include <stdio.h>
#include <Vk/VkGang.h>

VkComponent *
createGangedGroup(char *name, Widget parent)
{
  return new VkGang(name, parent);
}
