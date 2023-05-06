///////////////////////////
// ColorWindow.h
///////////////////////////

#include <Vk/VkSimpleWindow.h>
#include <Vk/VkCheckBox.h>

class ColorWindow: public VkSimpleWindow {

  public:
    ColorWindow (const char *);
    ~ColorWindow();
    virtual const char* className();

  private:
    void displayColor(char *);
    void colorChanged(VkCallbackObject *, void *, void *);

    static String _defaultResources[]; // Default resource values
    static String _colors[];           // Array of possible resulting colors
    Widget _resultColor;               // Label to display resulting color
    VkCheckBox *_primaries;            // Checkbox for setting colors
    int _colorStatus;                  // Bit-wise color status variable
                                       //     Bit 0: Cyan
                                       //     Bit 1: Magenta
                                       //     Bit 2: Yellow
                                       //   Also used as index into _colors[]
};
