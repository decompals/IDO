#include <math.h>
#include <Vk/VkPie.h>

VkComponent *
createPie(char *name, Widget parent)
{
  VkPie *pie;

  pie = new VkPie(name, parent);
  pie->setTitle("VkPie");
  pie->setRadius(100);
  pie->reset(100, 10);
  pie->add(10, 1, "red", "label1");
  pie->add(20, 1, "green", "label2");
  pie->add(30, 1, "blue", "label3");
  pie->add(10, 1, "purple", "label4");
  pie->update();

  return pie;
}
