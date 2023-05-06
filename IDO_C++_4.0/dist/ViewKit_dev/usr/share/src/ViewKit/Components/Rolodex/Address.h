/////////////////////////////////////////////
// Address.h: Provide a display for a
//            name/addr/phone info screen
////////////////////////////////////////////

#ifndef ADDRESS_H
#define ADDRESS_H

#include <Vk/VkComponent.h>

class Record;
class LabeledText;

// The Address class combines three LabeledText field to form
// an input area/display area for address info

class Address : public VkComponent {

 private:

    Widget _frame, _rc;
    
    LabeledText *_nameField;
    LabeledText *_addr;
    LabeledText *_phone;

  public :

    Address(Widget, char *);
    ~Address();

    void clear();
    Record *getRecord();
    void display(Record*);
};

#endif
