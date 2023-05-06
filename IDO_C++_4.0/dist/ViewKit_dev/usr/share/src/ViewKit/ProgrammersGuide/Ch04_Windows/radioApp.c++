///////////////////////////
// radioApp.c++
///////////////////////////

#include <Vk/VkApp.h>
#include "RadioWindow.h"

void main ( int argc, char **argv )
{
    VkApp *radioApp = new VkApp("RadioApp", &argc, argv);
    RadioWindow *radioWin = new RadioWindow("radioWin");

    radioWin->show();
    radioApp->run();
}
