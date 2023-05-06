///////////////////////////
// ScaleWindow.h
///////////////////////////

#include <Vk/VkSimpleWindow.h>

class ScaleWindow: public VkSimpleWindow {

  public:
    ScaleWindow (const char *);
    ~ScaleWindow();
    virtual const char* className();

  private:
    static String _defaultResources[];
};
