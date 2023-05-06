#include <math.h>
#include <Vk/VkMeter.h>

VkComponent *
createMeter(char *name, Widget parent)
{
  VkMeter *meter;

  meter = new VkMeter(name, parent);
  meter->setTitle("VkMeter");
  meter->reset(100, 10);
  meter->add(20, 1, "red", "label1");
  meter->add(30, 1, "green", "label2");
  meter->add(40, 1, "blue", "label3");
  meter->update();

  return meter;
}
