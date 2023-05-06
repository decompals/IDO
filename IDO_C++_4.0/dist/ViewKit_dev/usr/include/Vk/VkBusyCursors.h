///////////////////////////////////////////////////
// VkBusyCursors.h
///////////////////////////////////////////////////

#ifndef VKBUSYCURSORS_H
#define VKBUSYCURSORS_H

#include <Vk/VkCursorList.h>

class VkBusyCursors : public VkCursorList {
    
  public:
    
    VkBusyCursors( );

  protected:
    
    void createCursor(int index);   // Overrides base class' pure virtual

  private:
    
    XColor  xcolors[2];    
};


#endif
