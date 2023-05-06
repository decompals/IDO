////////////////////////////////////////
// Rolodex.C
///////////////////////////////////////
#include "Rolodex.h"
#include "Address.h"
#include "Command.h"
#include "Database.h"
#include <Xm/MainW.h>


// The Rolodex constructor creates a widget and then instantiates
// the various subcomponents that form the rolodex UI.


Rolodex::Rolodex(Widget parent, char *name) : VkComponent(name)
{

    // Create the base widget as a main window widget to hold 
    // all other components

    _baseWidget = XmCreateMainWindow(parent, _name, NULL, 0);

    // Create the three sub-components

    _addr     = new Address( _baseWidget, "address");
    _database = new Database( _addr );
    _commands = new Command( _baseWidget, "command", _addr, _database);

    // Install the base widgets of each sub-component in the 
    // appropriate place in the main window

    XtVaSetValues(_baseWidget, 
		  XmNworkWindow,             _addr->baseWidget(),
		  XmNcommandWindow,          _commands->baseWidget(),
		  XmNcommandWindowLocation,  XmCOMMAND_BELOW_WORKSPACE,
		  NULL);

    _addr->show();
    _commands->show();
}


Rolodex::~Rolodex()
{
    delete _addr;
    delete _database;
    delete _commands;
}
