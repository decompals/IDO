#include <Vk/VkTabPanel.h>

VkComponent *
createTabPanel(char *name, Widget parent)
{
  VkTabPanel *tp;

  tp = new VkTabPanel(name, parent);
  tp->addTab("one", NULL);
  tp->addTab("two", NULL);
  tp->addTab("three", NULL);
  tp->addTab("four", NULL);
  tp->addTab("five", NULL);

  return tp;
}
