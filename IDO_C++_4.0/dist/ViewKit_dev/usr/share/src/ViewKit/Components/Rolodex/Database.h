/////////////////////////////////////////////////////////////
// Record.h: Declare the interface to the rolodex database
////////////////////////////////////////////////////////////
#ifndef DATABASE_H
#define DATABASE_H

class Record;
class Address;

class Database {

  private:

    Address *_addr;

  public:

    Database(Address *);
    ~Database();

    void add(Record*);
    void remove(Record *);
};

#endif
