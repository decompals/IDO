#include <math.h>
#include <Vk/VkVuMeter.h>

VkComponent *
createVuMeter(char *name, Widget parent)
{
  VkVuMeter *meter;

  meter = new VkVuMeter(name, parent);
  meter->setValue(50, 100);

  return meter;
}
