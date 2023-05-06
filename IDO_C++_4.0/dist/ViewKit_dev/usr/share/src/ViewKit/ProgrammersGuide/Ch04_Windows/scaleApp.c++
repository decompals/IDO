///////////////////////////
// scaleApp.c++
///////////////////////////

#include <Vk/VkApp.h>
#include "ScaleWindow.h"

void main ( int argc, char **argv )
{
    VkApp *scaleApp = new VkApp("ScaleApp", &argc, argv);
    ScaleWindow *scaleWin = new ScaleWindow("scaleWin");

    scaleWin->show();
    scaleApp->run();
}
