//////////////////////////////////////////////////////////
// Command.C: Issue commands to other Rolodex components
//////////////////////////////////////////////////////////
#include "Command.h"
#include "Address.h"
#include "Database.h"
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>

Command::Command ( Widget    parent, 
		   char     *name, 
		   Address  *addr,
		   Database *db) : VkComponent(name)
{
    // Remember the Address and Database objects for later use

    _addr = addr;
    _db   = db;

    // Create the base widget

    _baseWidget = XtVaCreateWidget ( _name, xmRowColumnWidgetClass, 
				     parent, 
				     XmNnumColumns, 1,
				     XmNorientation, XmHORIZONTAL, 
				     NULL );

    // The Command panel supports 3 buttons, with callbacks
    
    _clearBtn  = XtVaCreateManagedWidget ( "clear",  
					   xmPushButtonWidgetClass, 
					   _baseWidget, NULL, 0 );

    _addBtn    = XtVaCreateManagedWidget ( "add",    
					   xmPushButtonWidgetClass, 
					   _baseWidget, NULL, 0 );

    _removeBtn = XtVaCreateManagedWidget ( "delete", 
					   xmPushButtonWidgetClass, 
					   _baseWidget, NULL, 0 );

    XtAddCallback(_clearBtn,  XmNactivateCallback, 
		  &Command::clearCallback, (XtPointer) this);
    XtAddCallback(_addBtn,    XmNactivateCallback, 
		  &Command::addCallback, (XtPointer) this);
    XtAddCallback(_removeBtn, XmNactivateCallback, 
		  &Command::removeCallback, (XtPointer) this);
}

Command::~Command()
{
    // Empty
}

void Command::clearCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    Command * obj = (Command*) clientData;
    obj->clear(w, callData);
}

void Command::clear(Widget, XtPointer)
{
     _addr->clear();
}


void Command::addCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    Command * obj = (Command*) clientData;

    obj->add(w, callData);
}

void Command::add(Widget, XtPointer)
{
    _db->add(_addr->getRecord());
}

void Command::removeCallback(Widget w, XtPointer clientData, XtPointer callData)
{
    Command * obj = (Command*) clientData;

    obj->remove(w, callData);
}

void Command::remove(Widget, XtPointer)
{
    _db->remove(_addr->getRecord());
}




