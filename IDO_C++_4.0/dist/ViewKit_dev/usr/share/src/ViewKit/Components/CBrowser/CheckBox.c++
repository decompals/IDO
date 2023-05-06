#include <Vk/VkCheckBox.h>

VkComponent *
createCheckBox(char *name, Widget parent)
{
  VkCheckBox *box;

  box = new VkCheckBox(name, parent);

  box->addItem("Item One", 1);
  box->addItem("Item Two");
  box->addItem("Item Three");
  box->addItem("Item Four");
  box->addItem("Item Five");

  return box;
}
