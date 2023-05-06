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
//
// New-entry and edit-entry dialog.  Prompt the user for a new item's
// information, and add it to the database.
//

#include <stdio.h>
#include <string.h>
#include "EntryDialog.h"
#include "Entry.h"
#include "Entries.h"
#include "Data.h"
#include <Vk/VkPrefItem.h>
#include <Vk/VkWarningDialog.h>

EntryDialog::EntryDialog(const char *name, Entry *e, Entries *es) : VkPrefDialog(name)
{
    entry = e;
    entries = es;

    doSelect = False;

    // Create a preference hierarchy that consists of a VkPrefGroup
    // containing four VkPrefText's.

    group = new VkPrefGroup("entryDialog", False, True);
    nameF = new VkPrefText("name", 60);
    phoneF = new VkPrefText("phone", 60);
    addrF = new VkPrefText("addr", 60);
    commentF = new VkPrefText("comment", 60);
    group->addItem(nameF);
    group->addItem(phoneF);
    group->addItem(addrF);
    group->addItem(commentF);
    
    // Set the dialog's item, and fill in the default text values
    
    setItem(group);
    updateDisplay();
}

EntryDialog::~EntryDialog()
{
    group->deleteChildren();
    delete group;
}

// Update the preference item displays.  If we are showing an entry,
// fill in the correct strings.  Otherwise, initialize everything
// to blanks.

void EntryDialog::updateDisplay()
{
    if (entry)
    {
	nameF->setValue(entry->getName());
	phoneF->setValue(entry->getPhone());
	addrF->setValue(entry->getAddr());
	commentF->setValue(entry->getComment());
	group->updateValue();
    }
    else
    {
	nameF->setValue("");
	phoneF->setValue("");
	addrF->setValue("");
	commentF->setValue("");
	group->updateValue();
    }
}

// When the user presses OK, check the validity of the entry before
// continuing.  This way, we can keep the dialog up if there was
// an error.

void EntryDialog::ok(Widget w, XtPointer data)
{
    char ch;
    
    ch = *nameF->getValue();
    if (!((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')))
    {
	theWarningDialog->post("The name must begin with a letter.");
	return;
    }
    VkPrefDialog::ok(w, data);
    postOp();
}

// When the users causes an Apply by pressing the Apply or OK buttons,
// check the validity of the entry and the change the entry attributes.

void EntryDialog::apply(Widget w, XtPointer data)
{
    char ch;

    ch = *nameF->getValue();

    if (!((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')))
    {
	theWarningDialog->post("The name must begin with a letter.");
	return;
    }
    if (entry)
    {
	if (strcmp(nameF->getValue(), entry->element()->name))
	    doSelect = True;
	
	entry->changeInfo(nameF->getValue(), phoneF->getValue(),
			  addrF->getValue(), commentF->getValue());
    }
    else
    {
	doSelect = True;
	theData->addEntry(nameF->getValue(), phoneF->getValue(),
			  addrF->getValue(), commentF->getValue());
	updateDisplay();
    }
    VkPrefDialog::apply(w, data);
}

// When the user presses Cancel, do the default action as well
// as our own.

void EntryDialog::cancel(Widget w, XtPointer data)
{
    VkPrefDialog::cancel(w, data);
    postOp();
}

// After a change, cause a redisplay so that the correct parameters
// are used.

void EntryDialog::postOp()
{
    if (doSelect) 
      entries->reselect();
}
