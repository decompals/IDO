///////////////////////////
// RadioWindow2.c++
///////////////////////////

#include "RadioWindow2.h"
#include <Vk/VkRadioBox.h>

String RadioWindow::_defaultResources[] = {
    "*color*label*labelString:  Color",
    "*red.labelString:    Red",
    "*green.labelString:  Green",
    "*blue.labelString:   Blue",
    NULL };

RadioWindow::RadioWindow (const char *name) : VkSimpleWindow (name)
{
    // Empty
}

RadioWindow::~RadioWindow()
{
    // Empty
}

const char* RadioWindow::className()
{
    return "RadioWindow";
}

Widget RadioWindow::setUpInterface (Widget parent)
{
    setDefaultResources(mainWindowWidget(), _defaultResources);
    
    VkRadioBox *rb = new VkRadioBox( "color", parent );

    rb->addItem("red");
    rb->addItem("green");
    rb->addItem("blue");

    return(*rb);
}
