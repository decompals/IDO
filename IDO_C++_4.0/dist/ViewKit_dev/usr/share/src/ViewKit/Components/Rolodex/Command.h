//////////////////////////////////////////////////////////
// Command.h: A component that allows the user to issue 
//            commands in the rolodex
//////////////////////////////////////////////////////////
#ifndef Command_H
#define Command_H

#include <Vk/VkComponent.h>

class Address;
class Database;

class Command : public VkComponent {

  private:

    Address  *_addr;
    Database *_db;

    static void clearCallback(Widget,  XtPointer, XtPointer);
    static void addCallback(Widget,    XtPointer, XtPointer);
    static void removeCallback(Widget, XtPointer, XtPointer);

    Widget _clearBtn, _addBtn, _removeBtn;

    void clear(Widget, XtPointer);
    void add(Widget, XtPointer);
    void remove(Widget, XtPointer);
    
  public:
    
    Command ( Widget, char *, Address *addr, Database * );
   ~Command();
};

#endif   
