#include <math.h>
#include <Vk/VkFileSet.h>

VkComponent *
createFileSet(char *name, Widget parent)
{
  VkFileSet *fileSet;

  fileSet = new VkFileSet(parent, name);

  return fileSet;
}
