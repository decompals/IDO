///////////////////////////
// ColorWindow.c++
///////////////////////////

#include "ColorWindow.h"
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/LabelG.h>
#include <Vk/VkCheckBox.h>
#include <Vk/VkResource.h>

// Default ColorWindow class resource values.

String ColorWindow::_defaultResources[] = {
    "*windowTitle:                  Color Mixer",
    "*iconTitle:                    Color Mixer",
    "*primaries*label*labelString:  Primary Colors",
    "*cyan.labelString:             Cyan",
    "*magenta.labelString:          Magenta",
    "*yellow.labelString:           Yellow",
    "*resultLabel.labelString:      Resulting Color",
    "*cyan:                         Cyan",
    "*magenta:                      Magenta",
    "*yellow:                       Yellow",
    "*blue:                         Blue",
    "*red:                          Red",
    "*green:                        Green",
    "*white:                        White",
    "*black:                        Black",
    NULL };

// Set _colors array to correspond to color values indicated by the
// bits in the _colorStatus variable.

String ColorWindow::_colors[] = {
    "white",
    "cyan",
    "magenta",
    "blue",
    "yellow",
    "green",
    "red",
    "black" };

ColorWindow::ColorWindow (const char *name) : VkSimpleWindow (name)
{
    Arg args[5];
    int n;

    // Set default resources for the window.

    setDefaultResources(mainWindowWidget(), _defaultResources);
    
    // Create a Form widget to use as the window's view.

    Widget _form = XmCreateForm(mainWindowWidget(), "form", NULL, 0);

    
    // Create a VkCheckBox object to allow users to select primary colors.
    // Add toggle buttons and set their intial values to FALSE (unselected).
    // The labels for the checkbox frame and the toggle buttons are set
    // by the resouce database.
    
    _primaries = new VkCheckBox( "primaries", _form );
    _primaries->addItem("cyan", FALSE);
    _primaries->addItem("magenta", FALSE);
    _primaries->addItem("yellow", FALSE);
    _primaries->addCallback(VkCheckBox::itemChangedCallback, this,
                            (VkCallbackMethod) &ColorWindow::colorChanged);
    _primaries->show();
    
    // Set constraint resources on checkbox's base widget.
    
    n = 0;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetValues(_primaries->baseWidget(), args, n);

    // Create a frame to display the name of the resulting blended color.

    n = 0;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNleftWidget, _primaries->baseWidget()); n++;
    Widget _result = XmCreateFrame(_form, "result", args, n);
    XtManageChild(_result);
    
    // Create a frame title label.  The label text is set by the resource
    // database.
    
    n = 0;
    XtSetArg(args[n], XmNchildType, XmFRAME_TITLE_CHILD); n++;
    Widget _resultLabel = XmCreateLabelGadget( _result, "resultLabel", args, n);
    
    // Create the label to display the blended color name.
    
    _resultColor = XmCreateLabelGadget( _result, "resultColor", NULL, 0);
    
    // Set intial value of _colorStatus and label string to white (all off).
    
    _colorStatus = 0;
    displayColor(_colors[_colorStatus]);
    
    XtManageChild(_resultLabel);
    XtManageChild(_resultColor);

    // Add the top-level Form widget as the window's view.

    addView(_form);
    
    // Set the window title and the icon title.
    
    setTitle("windowTitle");
    setIconName("iconTitle");
}

ColorWindow::~ColorWindow()
{
    // Empty
}

const char* ColorWindow::className()
{
    return "ColorWindow";
}

// Given a color name, update the label to display the color

void ColorWindow::displayColor(char *newColor)
{
    Arg args[2];
    int n;

    // Common resource trick in ViewKit applications.
    // Given a string, check the resource database for a corresponding
    // value.  If none exists, use the string as the value.

    char *_colorName = (char *) VkGetResource(_baseWidget, newColor, "Color",
                                              XmRString, newColor);

    // Update the label

    XmString _label = XmStringCreateSimple(_colorName);
    n = 0;
    XtSetArg(args[n], XmNlabelString, _label); n++;
    XtSetValues(_resultColor, args, n);
    XmStringFree(_label);
}

// When the user changes the value of one of the toggles, update the
// display to show the new blended color.

void ColorWindow::colorChanged(VkCallbackObject *obj, void *, void *callData)
{
    ColorWindow *win = (ColorWindow *) obj;
    int index = (int) callData;
    
    // Update color status based on toggle value.  Set or reset the
    // status bit corresponding to the respective toggle.

    if (_primaries->getValue(index))
        _colorStatus |= 1<<index;
    else
        _colorStatus &= ~(1<<index);

    // Update the display to show the new blended color, using
    // _colorStatus as an index.

    displayColor(_colors[_colorStatus]);
}
