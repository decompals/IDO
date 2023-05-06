#ifndef _C_DATA_
#define _C_DATA_

#include <Vk/VkComponent.h>

typedef VkComponent* (*ComponentProc)(char *name, Widget parent);

typedef struct {
  char *name;
  ComponentProc proc;
  Boolean omitFromOverview;
} ComponentInfo;

extern ComponentInfo componentInfo[];
extern int componentInfoSize;

#endif
