///////////////////////////
// RadioWindow.c++
///////////////////////////

#include "RadioWindow.h"
#include <Vk/VkRadioBox.h>

String RadioWindow::_defaultResources[] = {
    "*color*label*labelString:  Color",
    "*red.labelString:    Red",
    "*green.labelString:  Green",
    "*blue.labelString:   Blue",
    NULL };

RadioWindow::RadioWindow (const char *name) : VkSimpleWindow (name)
{
    setDefaultResources(mainWindowWidget(), _defaultResources);
    
    VkRadioBox *rb = new VkRadioBox( "color", mainWindowWidget() );

    rb->addItem("red");
    rb->addItem("green");
    rb->addItem("blue");

    addView(rb);
}

RadioWindow::~RadioWindow()
{
    // Empty
}

const char* RadioWindow::className()
{
    return "RadioWindow";
}
