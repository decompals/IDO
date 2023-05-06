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
// Display a preferences dialog with the current preferences settings,
// and allow the user to modify these settings.
//

#include <stdio.h>
#include <stdlib.h>
#include "PhoneBook.h"
#include "PrefDialog.h"
#include "Preferences.h"
#include <Vk/VkPrefItem.h>
#include <Vk/VkWarningDialog.h>

PrefDialog::PrefDialog(const char *name, PhoneBook *o) : VkPrefDialog(name)
{
    owner = o;

    // Create our preferences items.  The hierarchy consists of a single
    // VkPrefGroup containing three VkPrefText's and two VkPrefToggle's.

    group = new VkPrefGroup("preferences");
    filename = new VkPrefText("filename", 40);
    showAddr = new VkPrefToggle("showAddr");
    showComment = new VkPrefToggle("showComment");
    itemSpacing = new VkPrefText("itemSpacing", 5);
    printWidth = new VkPrefText("printWidth", 5);
    
    group->addItem(filename);
    group->addItem(showAddr);
    group->addItem(showComment);
    group->addItem(itemSpacing);
    group->addItem(printWidth);
    
    // Set our dialog's preference item and transfer our preference
    // settings to the VkPrefItem's
    
    setItem(group);
    updateDisplay();
}

PrefDialog::~PrefDialog()
{
    group->deleteChildren();
    delete group;
}

// Use the preference settings from the global Preferences object to
// set the values in the VkPrefItem's.

void PrefDialog::updateDisplay()
{
    char str[256];
    
    filename->setValue(thePreferences->filename());
    showAddr->setValue(thePreferences->showAddr());
    showComment->setValue(thePreferences->showAddr());
    sprintf(str, "%d", thePreferences->itemSpacing());
    itemSpacing->setValue(str);
    sprintf(str, "%d", thePreferences->printWidth());
    printWidth->setValue(str);

    group->updateValue();
}

// When the user causes Apply by pressing the Apply or OK buttons,
// check each VkPrefItem for a change.  If changed, set the appropriate
// setting in the global Preferences object.

void PrefDialog::apply(Widget, XtPointer)
{
    Boolean changed;
    int num;
    
    changed = False;
    if (filename->changed())
    {
	thePreferences->setFilename(filename->getValue());
	changed = True;
    }
    
    if (itemSpacing->changed())
    {
	if ((num = atoi(itemSpacing->getValue())) >= 0)
	{
	    thePreferences->setItemSpacing(num);
	    changed = True;
	}
	else
	{
	    theWarningDialog->post("Illegal item spacing value.");
	}
    }
    
    if (printWidth->changed())
    {
	if ((num = atoi(printWidth->getValue())) >= 0)
	{
	    thePreferences->setPrintWidth(num);
	    changed = True;
	}
	else
	{
	    theWarningDialog->post("Illegal print width value.");
	}
    }
    if (showAddr->changed())
    {
	thePreferences->setShowAddr(showAddr->getValue());
	changed = True;
    }
    if (showComment->changed())
    {
	thePreferences->setShowComment(showComment->getValue());
	changed = True;
    }

    if (changed)
    {
	owner->prefChanged();
    }
}

