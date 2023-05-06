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
#ifndef ENTRY_DIALOG_H
#define ENTRY_DIALOG_H

#include <Vk/VkPrefDialog.h>

class VkPrefGroup;
class VkPrefText;
class Entry;
class Entries;

class EntryDialog : public VkPrefDialog {
  
  public:
  
    EntryDialog(const char *name, Entry *entry, Entries *entries);
    ~EntryDialog();
    
    void updateDisplay();

  protected:
    
    virtual void ok(Widget w, XtPointer data);
    virtual void apply(Widget w, XtPointer data);
    virtual void cancel(Widget w, XtPointer data);
    void postOp();
    
    Entry *entry;
    Entries *entries;
    VkPrefGroup *group;
    VkPrefText *nameF, *phoneF, *addrF, *commentF;
    Boolean doSelect;
};

#endif
