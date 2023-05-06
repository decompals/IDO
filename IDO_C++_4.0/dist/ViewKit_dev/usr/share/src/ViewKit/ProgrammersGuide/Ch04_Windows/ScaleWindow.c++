///////////////////////////
// ScaleWindow.c++
///////////////////////////

#include "ScaleWindow.h"
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>

String ScaleWindow::_defaultResources[] = {
    "*scales.orientation:	VERTICAL",
    "*dayScale.titleString:	Days",
    "*dayScale.orientation:	HORIZONTAL",
    "*dayScale.maximum:		7",
    "*dayScale.minimum:		1",
    "*dayScale.value:		1",
    "*dayScale.showValue:	True",
    "*weekScale.titleString:	Weeks",
    "*weekScale.orientation:	HORIZONTAL",
    "*weekScale.maximum:	52",
    "*weekScale.minimum:	1",
    "*weekScale.value:		1",
    "*weekScale.showValue:	True",
    "*monthScale.titleString:	Months",
    "*monthScale.orientation:	HORIZONTAL",
    "*monthScale.maximum:	12",
    "*monthScale.minimum:	1",
    "*monthScale.value:		1",
    "*monthScale.showValue:	True",
    NULL };

ScaleWindow::ScaleWindow (const char *name) : VkSimpleWindow (name)
{
    setDefaultResources(mainWindowWidget(), _defaultResources);
    
    Widget scales = XtCreateWidget("scales", xmRowColumnWidgetClass,
                                   mainWindowWidget(), NULL, 0);
    
    Widget dayScale = XtCreateManagedWidget("dayScale", xmScaleWidgetClass,
                                            scales, NULL, 0);

    Widget weekScale = XtCreateManagedWidget("weekScale", xmScaleWidgetClass,
                                             scales, NULL, 0);

    Widget monthScale = XtCreateManagedWidget("monthScale", xmScaleWidgetClass,
                                              scales, NULL, 0);

    addView(scales);    
}

ScaleWindow::~ScaleWindow()
{
    // Empty
}

const char* ScaleWindow::className()
{
    return "ScaleWindow";
}
