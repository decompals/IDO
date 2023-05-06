///////////////////////////
// colors.c++
///////////////////////////

#include <Vk/VkApp.h>
#include "ColorWindow.h"

void main ( int argc, char **argv )
{
    VkApp *colorApp = new VkApp("ColorApp", &argc, argv);
    ColorWindow *colorWin = new ColorWindow("colorWin");

    colorWin->show();
    colorApp->run();
}
