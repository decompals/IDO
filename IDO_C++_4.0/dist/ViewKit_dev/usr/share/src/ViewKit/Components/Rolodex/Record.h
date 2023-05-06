/////////////////////////////////////////////////////////////
// Record.h: Declare a class to represent a 
// name/addr/phone record.
////////////////////////////////////////////////////////////

#ifndef RECORD_H
#define RECORD_H

#include <string.h>

class Record {

  protected:

    char *_name;
    char *_addr;
    char *_phone;

  public:

    Record(char *name, char *addr, char *phone) 
    { 
	_name  = strdup(name); 
	_addr  = strdup(addr); 
	_phone = strdup(phone);
    }

    ~Record();

    char *name() { return _name; }
    char *address() { return _addr; }
    char *phone() { return _phone; }
};

#endif
    
