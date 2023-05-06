////////////////////////////////////////////////////////////
// Rolodex.h: Declaration of the Rolodex component class
///////////////////////////////////////////////////////////

#ifndef ROLODEX_H
#define ROLODEX_H

#include <Vk/VkComponent.h>

class Address;
class Command;
class Database;

// Rolodex is a simple component that just combines 
// other components to form the complete interface.

class Rolodex : public VkComponent {

  private:

    Address  *_addr;
    Command  *_commands;
    Database *_database;

  public:

    Rolodex(Widget, char *);
    ~Rolodex();
};

#endif
