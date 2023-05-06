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
// This component displays a single database entry (name, phone,
// address, comment).  The parent Entries object is notified
// when the user right-buttons on the entry to popup the menu.  We
// also keep track of our entry dialog.
//

#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include "Entry.h"
#include "Entries.h"
#include "EntryDialog.h"
#include "Preferences.h"

#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/LabelG.h>

static int count;
static Arg args[10];

Entry::Entry(const char *name, Widget parent, Entries *e) : VkComponent(name)
{
    entries = e;
    dialog = NULL;

    nameV = strdup("");
    phoneV = strdup("");
    addrV = strdup("");
    commentV = strdup("");
    elementV = NULL;

    // Create a row column for our three lines.  The first (header) line
    // contains the name and phone number.  The next two lines contain
    // the address and comment.  These last two lines are only displayed
    // as necessary.

    count = 0;
    XtSetArg(args[count], XmNorientation, XmVERTICAL);  count++;
    XtSetArg(args[count], XmNpacking, XmPACK_TIGHT);  count++;
    _baseWidget = XmCreateRowColumn(parent, (char *) name, args, count);
    XtAddEventHandler(_baseWidget, ButtonPressMask, False,
		      Entry::buttonHandler, (XtPointer) this);

    count = 0;
    header = XmCreateForm(_baseWidget, "header", args, count);
    XtAddEventHandler(header, ButtonPressMask, False,
		      Entry::buttonHandler, (XtPointer) this);

    count = 0;
    XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNrightAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNfontList, entries->normalFont());  count++;
    phoneF = XmCreateLabelGadget(header, "phone", args, count);
    
    count = 0;
    XtSetArg(args[count], XmNtopAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNleftAttachment, XmATTACH_FORM);  count++;
    XtSetArg(args[count], XmNrightAttachment, XmATTACH_WIDGET);  count++;
    XtSetArg(args[count], XmNrightWidget, phoneF);  count++;
    XtSetArg(args[count], XmNfontList, entries->normalFont());  count++;
    nameF = XmCreateLabelGadget(header, "name", args, count);
    
    count = 0;
    XtSetArg(args[count], XmNfontList, entries->normalFont());  count++;
    addrF = XmCreateLabelGadget(_baseWidget, "addr", args, count);
    
    count = 0;
    XtSetArg(args[count], XmNfontList, entries->normalFont());  count++;
    commentF = XmCreateLabelGadget(_baseWidget, "comment", args, count);
    
    XtManageChild(nameF);
    XtManageChild(phoneF);
    XtManageChild(header);
}

Entry::~Entry()
{
    clearDialog();
    XtRemoveEventHandler(_baseWidget, ButtonPressMask, False,
			 Entry::buttonHandler, (XtPointer) this);
    XtRemoveEventHandler(header, ButtonPressMask, False,
			 Entry::buttonHandler, (XtPointer) this);
    free(nameV);
    free(phoneV);
    free(addrV);
    free(commentV);
}

const char *Entry::className()
{
    return "Entry";
}
  
// When our database element changes completely, get rid of the old entry
// dialog and display the latest information.

void Entry::setInfo(DataElement *element)
{
    clearDialog();
    updateInfo(element);
}

// When our info changes from the entry dialog, update the database
// and also display the latest information.

void Entry::changeInfo(char *name, char *phone, char *addr, char *comment)
{
    theData->changeEntry(elementV, name, phone, addr, comment);
    updateInfo(elementV);
}

// Return our entry dialog, creating it if necessary and updating it
// with the latest information.

EntryDialog *Entry::entryDialog()
{
    if (!dialog) 
	dialog = new EntryDialog("entryDialog", this, entries);
    else
	dialog->updateDisplay();

    return (dialog);
}

// Get rid of our entry dialog.

void Entry::clearDialog()
{
    if (dialog)
    {
	delete dialog;
	dialog = NULL;
    }
}


// Display the latest information in our label widgets, getting it from
// our database element.

void Entry::updateInfo(DataElement *element)
{
    free(nameV);
    free(phoneV);
    free(addrV);
    free(commentV);
    nameV = strdup(element->name);
    phoneV = strdup(element->phone);
    addrV = strdup(element->addr);
    commentV = strdup(element->comment);
    setLabel(nameF, nameV, 0);
    setLabel(phoneF, phoneV, 0);
    if (thePreferences->showAddr() && strlen(addrV))
    {
	setLabel(addrF, addrV, 5);
	XtManageChild(addrF);
    }
    else
    {
	XtUnmanageChild(addrF);
    }
    if (thePreferences->showComment() && strlen(commentV))
    {
	setLabel(commentF, commentV, 5);
	XtManageChild(commentF);
    }
    else
    {
	XtUnmanageChild(commentF);
    }

    elementV = element;
}

// Set a Motif label string, with a possible indentation

void Entry::setLabel(Widget label, char *value, int indent)
{
    XmString xs;
    char *str;
    int each;

    if (!indent)
    {
	xs = XmStringCreateSimple(value);
    }
    else
    {
	str = new char[strlen(value)+indent+1];
	for (each=0; each<indent; each++)
	{
	    str[each] = ' ';
	}
	strcpy(str+indent, value);
	xs = XmStringCreateSimple(str);
	delete[] str;
    }
    
    count = 0;
    XtSetArg(args[count], XmNlabelString, xs);  count++;
    XtSetValues(label, args, count);
    XmStringFree(xs);
}

// Post our popup menu

void Entry::menu(XEvent *event)
{
    entries->postMenu(this, event);
}


void Entry::buttonHandler(Widget, XtPointer clientData, XEvent *event, Boolean *)
{
    Entry *obj = (Entry *) clientData;

    if (event->xbutton.button == Button3)
	obj->menu(event);
}
