#include <Vk/VkRadioBox.h>

VkComponent *
createRadioBox(char *name, Widget parent)
{
  VkRadioBox *box;

  box = new VkRadioBox(name, parent);

  box->addItem("Item One", 1);
  box->addItem("Item Two");
  box->addItem("Item Three");
  box->addItem("Item Four");
  box->addItem("Item Five");

  return box;
}
