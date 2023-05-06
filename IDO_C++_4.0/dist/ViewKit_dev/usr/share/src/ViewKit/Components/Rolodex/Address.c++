////////////////////////////////////////
// Address.C
///////////////////////////////////////
#include "Address.h"
#include "LabeledText.h"
#include "Record.h"
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>

Address::Address(Widget parent, char *name) : VkComponent(name)
{
    _baseWidget = XtCreateWidget(_name, xmFormWidgetClass, 
				 parent, NULL, 0);

    _frame = XtVaCreateManagedWidget("frame", 
				     xmFrameWidgetClass,
				     _baseWidget, 
				     XmNbottomAttachment, XmATTACH_FORM,
				     XmNbottomOffset,     10,
				     XmNtopAttachment,    XmATTACH_FORM,
				     XmNtopOffset,        10,
				     XmNrightAttachment,  XmATTACH_FORM,
				     XmNrightOffset,      10,
				     XmNleftAttachment,   XmATTACH_FORM,
				     XmNleftOffset,       10,
				     NULL);

    _rc = XtVaCreateManagedWidget("rc", 
				  xmRowColumnWidgetClass,
				  _frame,
				  XmNnumColumns,    1,
				  XmNorientation,   XmVERTICAL,
				  XmNadjustLast,    FALSE,
				  NULL );

    // Add a label/text field for each entry

    _nameField = new LabeledText(_rc, "name");
    _addr      = new LabeledText(_rc, "addr");
    _phone     = new LabeledText(_rc, "phone");

    _nameField->show();
    _addr->show();
    _phone->show();
}

Address::~Address()
{
    // Empty
}

void Address::clear()
{
    // Erase all fields

    _nameField->setString("");
    _addr->setString("");
    _phone->setString("");
}

Record *Address::getRecord()
{
    Record *r = new Record (_nameField->getString(),
			    _addr->getString(),
			    _phone->getString());
    return r;
}

void Address::display(Record *r)
{
    _nameField->setString(r->name());
    _addr->setString(r->address());
    _phone->setString(r->phone());
}





