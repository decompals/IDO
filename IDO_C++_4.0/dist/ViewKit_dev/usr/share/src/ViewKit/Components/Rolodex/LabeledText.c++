///////////////////////////////////////////////////////
// LabeledText.C: A simple C++ component class
//////////////////////////////////////////////////////// 
#include "LabeledText.h"
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/TextF.h>

LabeledText::LabeledText ( Widget parent, char * name ) : VkComponent(name)
{
    _baseWidget = XtCreateManagedWidget ( _name, 
					 xmRowColumnWidgetClass, 
					 parent, NULL, 0 );

    _label     = XtCreateManagedWidget ( "label", 
					  xmLabelWidgetClass,
					  _baseWidget,  
					NULL, 0);

    _text      = XtCreateManagedWidget ( "text", 
					xmTextFieldWidgetClass,
					_baseWidget,  
					NULL, 0);
}

LabeledText::~LabeledText ( )
{
    // Empty
}

char *LabeledText::getString()
{
    return XmTextFieldGetString(_text);
}

void LabeledText::setString( char * txt)
{
    XmTextFieldSetString( _text, txt);
}


