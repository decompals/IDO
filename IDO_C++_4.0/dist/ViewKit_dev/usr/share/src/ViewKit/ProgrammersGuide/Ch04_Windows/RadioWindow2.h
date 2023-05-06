///////////////////////////
// RadioWindow2.h
///////////////////////////

#include <Vk/VkSimpleWindow.h>

class RadioWindow: public VkSimpleWindow {

  public:
    RadioWindow (const char *);
    ~RadioWindow();
    virtual const char* className();

  protected:
    Widget setUpInterface(Widget);

  private:
    static String _defaultResources[];
};
