////////////////////////////////////////////////////////////////////////////////
///////   Copyright 1992, Silicon Graphics, Inc.  All Rights Reserved.   ///////
//                                                                            //
// This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;     //
// the contents of this file may not be disclosed to third parties, copied    //
// or duplicated in any form, in whole or in part, without the prior written  //
// permission of Silicon Graphics, Inc.                                       //
//                                                                            //
// RESTRICTED RIGHTS LEGEND:                                                  //
// Use,duplication or disclosure by the Government is subject to restrictions //
// as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data     //
// and Computer Software clause at DFARS 252.227-7013, and/or in similar or   //
// successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -    //
// rights reserved under the Copyright Laws of the United States.             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
#ifndef DATA_H
#define DATA_H

#include <X11/Intrinsic.h>

class Data;

extern Data *theData;

typedef struct {
    char *name;
    char *phone;
    char *addr;
    char *comment;
} DataElement;

typedef struct _DataList {
    DataElement *element;
    struct _DataList *next;
} DataList;

class Data {
  public:
    Data();
    ~Data();

    Boolean openFile(char *filename);
    Boolean writeFile(char *filename);
    Boolean isDirty() { return dirty; }
    
    DataList *allData() { return head->next; }
    DataList *findLastName(char lastPrefix);
    void addEntry(char *name, char *phone, char *addr, char *comment);
    void removeEntry(DataElement *element);
    void changeEntry(DataElement *element,
		     char *name, char *phone, char *addr, char *comment);

 protected:
    char upper(char ch);
    void deleteData();
    void insertElement(DataElement *element);
    void resort(DataElement *element);

    DataList *head;
    Boolean dirty;
};

#endif
