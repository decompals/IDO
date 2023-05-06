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
// Show the selected database elements.  Use Entry components for each
// displayed item.
//

#include <stdio.h>
#include "Info.h"
#include "Entries.h"
#include "Entry.h"
#include "EntryDialog.h"
#include "Data.h"
#include "Preferences.h"
#include <Vk/VkPopupMenu.h>
#include <Vk/VkQuestionDialog.h>

#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>

static int count;
static Arg args[10];

Entries::Entries(const char *name, Widget parent, Info *i) : VkComponent(name)
{
    Widget w;

    numEntries = 0;
    realEntries = 0;
    sizeEntries = 8;
    entries = (Entry **) XtMalloc(sizeEntries*sizeof(Entry *));
    lastRange = strdup("");
    info = i;
    
    // Create a row column to contain all the entry components

    count = 0;
    XtSetArg(args[count], XmNorientation, XmVERTICAL);  count++;
    XtSetArg(args[count], XmNpacking, XmPACK_TIGHT);  count++;
    XtSetArg(args[count], XmNspacing, thePreferences->itemSpacing());  count++;
    _baseWidget = XmCreateRowColumn(parent, (char *) name, args, count);
    
    // Create the entry popup menu which we display when the user
    // right-buttons on an entry.
    
    entryPopup = new VkPopupMenu();
    entryPopup->addLabel("entryPopup");
    entryPopup->addSeparator();
    entryPopup->addAction("editEntryItem", &Entries::editCallback,
			  (XtPointer) this);
    entryPopup->addAction("deleteEntryItem", &Entries::deleteCallback,
			  (XtPointer) this);
    entryPopup->build(_baseWidget);
    count = 0;
    XtSetArg(args[count], XmNpopupEnabled, False);  count++;
    XtSetValues(entryPopup->baseWidget(), args, count);
    
    // Create an undisplayed button so that we can get a non-bold
    // font for the entry display
    
    count = 0;
    w = XmCreatePushButton(_baseWidget, "dummyButton", args, count);
    count = 0;
    XtSetArg(args[count], XmNfontList, &buttonFont);  count++;
    XtGetValues(w, args, count);
}

Entries::~Entries()
{
    XtFree((char *)lastRange);
    delete entryPopup;
    deleteAll();
    XtFree((char *)entries);
}

const char *Entries::className()
{
    return "Entries";
}
  
// The user has selected a range.  Add each letter in the range to
// our display.  After we're done, hide any excess Entry components.

void Entries::display(char *range)
{
    int each;
    char *r;
    
    r = strdup(range);
    XtUnmanageChild(_baseWidget);
    count = 0;
    XtSetArg(args[count], XmNspacing, thePreferences->itemSpacing());  count++;
    XtSetValues(_baseWidget, args, count);
    realEntries = 0;
    while (*range != '\0')
    {
	addDisplay(*range);
	range++;
    }

    for (each=realEntries; each<numEntries; each++)
    {
	entries[each]->clearDialog();
	entries[each]->hide();
  }
    
    numEntries = realEntries;
    XtManageChild(_baseWidget);
    
    XtFree((char *)lastRange);
    lastRange = r;
}

// Reselect the last range, to update the display after a preferences
// change or after an entry is deleted.

void Entries::reselect()
{
    info->selectRange(lastRange, False);
}

// Post the popup menu for a particular entry.

void Entries::postMenu(Entry *entry, XEvent *event)
{
    menuEntry = entry;
    XmMenuPosition(entryPopup->baseWidget(), &event->xbutton);
    XtManageChild(entryPopup->baseWidget());
}

// Delete all our entry components.

void Entries::deleteAll()
{
    int each;
    
    for (each=0; each<numEntries; each++)
	delete entries[each];

    numEntries = 0;
    realEntries = 0;
}

// Add a single letter to our display.  Ask the database for the elements,
// and use existing Entry components for their display.  If we run
// out of Entry components, create more.

void Entries::addDisplay(char letter)
{
    DataList *result, *ptr;
    Entry *entry;
    
    if (result = theData->findLastName(letter))
    {
	ptr = result;
	while (ptr)
	{
	    if (realEntries < numEntries)
	    {
		entry = entries[realEntries];
		entry->show();
	    }
	    else
	    {
		entry = addEntry("entry");
	    }
	    realEntries++;
	    entry->setInfo(ptr->element);
	    entry->show();
	    ptr = ptr->next;
	}
	XtFree((char *)result);
    }
}

// Create a new Entry component and add it to our list.

Entry *Entries::addEntry(const char *name)
{
    if (numEntries == sizeEntries)
    {
	sizeEntries = 2*sizeEntries;
	entries = (Entry **) XtRealloc((char *)entries, sizeEntries*sizeof(Entry *));
    }
    entries[numEntries] = new Entry(name, _baseWidget, this);
    numEntries++;
    return entries[numEntries-1];
}

// The user has selected Edit from the popup menu.  Get the entry
// dialog and show it.

void Entries::editMenu()
{
    EntryDialog *dialog;
    
    dialog = menuEntry->entryDialog();
    dialog->show();
}

// The user has selected Delete from the popup menu.  Confirm the deletion
// and then remove the entry from the database.  Force a redisplay.
// We don't delete the Entry component because it can be reused.

void Entries::deleteMenu()
{
    char str[1024];
    
    sprintf(str, "Are you sure you want to delete '%s'?", menuEntry->getName());
    if (theQuestionDialog->postAndWait(str) == VkDialogManager::OK)
    {
	theData->removeEntry(menuEntry->element());
	reselect();
    }
}


void Entries::editCallback(Widget, XtPointer clientData, XtPointer)
{
    Entries *obj = (Entries *) clientData;

    obj->editMenu();
}

void Entries::deleteCallback(Widget, XtPointer clientData, XtPointer)
{
    Entries *obj = (Entries *) clientData;
    
    obj->deleteMenu();
}

