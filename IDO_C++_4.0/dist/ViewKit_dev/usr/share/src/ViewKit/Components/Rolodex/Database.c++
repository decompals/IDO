/////////////////////////////////////////////////////////////
// Database.c++: Stub class that simulates a database.
//               This class simply prints the actions that 
//               would be taken by a real database.
////////////////////////////////////////////////////////////

#include "Database.h"
#include "Record.h"
#include <iostream.h>


Database::Database(Address * a)
{
    _addr = a;
}

Database::~Database()
{
    // Empty
}

void Database::add(Record *r)
{
    cout << "Adding: " << r->name() << "\n" << r->address() << "\n" << r->phone() <<"\n" << flush;
}

void Database::remove(Record *r)
{
    cout << "Removing: " << r->name() << "\n" << r->address() << "\n" << r->phone() << "\n" << flush;
}
