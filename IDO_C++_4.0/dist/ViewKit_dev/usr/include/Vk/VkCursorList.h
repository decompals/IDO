#ifndef VKCURSORLIST_H
#define VKCURSORLIST_H

#include <Xm/Xm.h>

class VkCursorList {
    
  public:
    
    virtual ~VkCursorList();
    
    Cursor next();
    Cursor current();
    void reset();

  protected:
    
    int       _numCursors;
    int       _current;   
    Pixmap   *_cursorList; 

    virtual void createCursor(int index) = 0; // Derived class must implement    
    VkCursorList ( int  );   
};
#endif
