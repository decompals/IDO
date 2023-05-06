///////////////////////////
// radioApp2.c++
///////////////////////////

#include <Vk/VkApp.h>
#include "RadioWindow2.h"

void main ( int argc, char **argv )
{
    VkApp *radioApp = new VkApp("RadioApp", &argc, argv);
    RadioWindow *radioWin = new RadioWindow("radioWin");

    radioWin->show();
    radioApp->run();
}
