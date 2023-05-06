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
#ifndef ENTRY_H
#define ENTRY_H

#include <Vk/VkComponent.h>
#include "Data.h"

class Entries;
class EntryDialog;

class Entry : public VkComponent {
  public:
    Entry(const char *name, Widget parent, Entries *entries);
    ~Entry();

    const char *className();

    void setInfo(DataElement *element);
    char *getName() { return nameV; }
    char *getPhone() { return phoneV; }
    char *getAddr() { return addrV; }
    char *getComment() { return commentV; }
    void changeInfo(char *name, char *phone, char *addr, char *comment);

    DataElement *element() { return elementV; }
    EntryDialog *entryDialog();
    void clearDialog();

  protected:
    void updateInfo(DataElement *element);
    void setLabel(Widget label, char *value, int indent);
    void menu(XEvent *event);
    
    static void buttonHandler(Widget w, XtPointer clientData, XEvent *event,
			      Boolean *dispatch);

    Widget header, nameF, phoneF, addrF, commentF;
    char *nameV, *phoneV, *addrV, *commentV;
    DataElement *elementV;
    Entries *entries;
    EntryDialog *dialog;
};

#endif
