#ifndef  VKACTION_H
#define VKACTION_H

#include <Xm/Xm.h> // For widget definition

class VkAction {

  public:
    
    VkAction(const char *name);
    virtual ~VkAction();

    void execute();

  protected:

    virtual void undoit() = 0;
    virtual void doit()   = 0;
    char *_name;


  private:

    static void undoCallback(Widget, XtPointer, XtPointer);
    
};

#endif

