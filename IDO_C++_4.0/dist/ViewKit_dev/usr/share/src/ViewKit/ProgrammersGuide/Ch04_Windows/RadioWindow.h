///////////////////////////
// RadioWindow.h
///////////////////////////

#include <Vk/VkSimpleWindow.h>

class RadioWindow: public VkSimpleWindow {

  public:
    RadioWindow (const char *);
    ~RadioWindow();
    virtual const char* className();

  private:
    static String _defaultResources[];
};
