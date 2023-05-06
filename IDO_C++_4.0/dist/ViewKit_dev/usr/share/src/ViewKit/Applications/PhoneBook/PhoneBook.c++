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
// The main PhoneBook window.  This class displays the menus and the
// components which show all the information.  It does the appropriate
// things when menu items are selected, and instantiates the global
// objects.
//

#include <stdio.h>
#include "PhoneBook.h"
#include "Info.h"
#include "Data.h"
#include "Entries.h"
#include "EntryDialog.h"
#include "Preferences.h"
#include "Print.h"
#include "PrefDialog.h"
#include <Vk/VkApp.h>
#include <Vk/VkSubMenu.h>
#include <Vk/VkFileSelectionDialog.h>
#include <Vk/VkWarningDialog.h>
#include <Vk/VkQuestionDialog.h>

#include <Xm/Form.h>

PhoneBook::PhoneBook(const char *name) : VkWindow(name)
{
    Widget form;
    
    newDialog  = NULL;
    print      = NULL;
    prefDialog = NULL;
    
    // Create a global Preferences object for preference settings and
    // initialize its settings
    
    pref = new Preferences(theApplication->baseWidget());
    pref->loadPreferences();
    
    // Create a global Data object to store our database information and
    // initialize its contents from disk
    
    new Data();
    theData->openFile(pref->filename());

    addFileMenu();
    
    form = XmCreateForm(mainWindowWidget(), "form", NULL, 0);
    
    info = new Info("info", form);
    
    XtVaSetValues(info->baseWidget(),
		  XmNtopAttachment, XmATTACH_FORM,
		  XmNbottomAttachment, XmATTACH_FORM,
		  XmNleftAttachment, XmATTACH_FORM,
		  XmNrightAttachment, XmATTACH_FORM,
		  NULL);
    
    info->show();
    
    addView(form);
}

PhoneBook::~PhoneBook()
{
    delete newDialog;
    delete print;
    delete prefDialog;
    
    delete pref;
    delete theData;
}

const char *PhoneBook::className()
{
    return "PhoneBook";
}

// When preference settings change, we need to redisplay our info
// in case formatting options changed.  We do this by telling the
// Entries object to reselect the currently-selected item.

void PhoneBook::prefChanged()
{
    info->getEntries()->reselect();
}

// When the user selects the Quit item from the window manager, we want
// to shut down the entries application.  The default behavior of
// handleWmDeleteMessage is to just close the window, but we want all
// windows to go away when this happens.

void PhoneBook::handleWmDeleteMessage()
{
    theApplication->quitYourself();
}

// Before allowing the application to quit, we check for unsaved
// changes to the database.  If there are any, we ask the user if
// they want to save these changes before exiting.  If they do want
// to save the changes but the write to disk fails, we abort the
// application quit by returning False.  The default behavior of
// okToQuit is to return True.

Boolean PhoneBook::okToQuit()
{
    if (theData->isDirty() || thePreferences->isDirty())
    {
	theQuestionDialog->setButtonLabels("Yes", "No");

	if( theQuestionDialog->postAndWait("There are unsaved changes.  Save before quitting?",
					   True, True, False,
					   NULL,
					   this) == VkDialogManager::OK)
	{
	    if (theData->writeFile(pref->filename()))
	    {
		pref->savePreferences();
	    }
	    else
	    {
		return False;
	    }
	}
    }
    
    return True;
}

// Create the File menu and its contents

void PhoneBook::addFileMenu()
{
    VkSubMenu *menu;

    menu = addMenuPane("fileMenu");
  
    menu->addAction("newMenuItem",
		    PhoneBook::newCallback,
		    (XtPointer) this);
    menu->addAction("prefMenuItem",
		    PhoneBook::prefCallback,
		    (XtPointer) this);
    menu->addSeparator();
    menu->addAction("loadMenuItem",
		    PhoneBook::loadCallback,
		    (XtPointer) this);
    menu->addAction("saveMenuItem",
		    PhoneBook::saveCallback,
		    (XtPointer) this);
    menu->addAction("saveAsMenuItem",
		    PhoneBook::saveasCallback,
		    (XtPointer) this);
    menu->addAction("printMenuItem",
		    PhoneBook::printCallback,
		    (XtPointer) this);
    menu->addSeparator();
    menu->addAction("quitMenuItem",
		    PhoneBook::quitCallback,
		    (XtPointer) this);
}

// Create and show the new-entry dialog when the user selects File...New

void PhoneBook::newMenuAction()
{
    if (!newDialog) 
	newDialog = new EntryDialog("newDialog", NULL, info->getEntries());
    else
	newDialog->updateDisplay();

    newDialog->show();
}

// Create and show the preferences dialog when the user selects
// File...Preferences

void PhoneBook::prefMenuAction()
{
    if (!prefDialog) 
	prefDialog = new PrefDialog("prefDialog", this);
    else 
	prefDialog->updateDisplay();

    prefDialog->show();
}

// Create and show a file selection dialog when the user selects
// File...Load.  Display a warning if we couldn't load the database
// from the selected filename.

void PhoneBook::loadMenuAction()
{
    char str[1024];

    theFileSelectionDialog->setTitle("Load Database");
    theFileSelectionDialog->setSelection(pref->filename());
    theFileSelectionDialog->postAndWait(NULL, "loadDialog", this);
    
    if (theFileSelectionDialog->fileName())
    {
	if (theData->openFile((char *) theFileSelectionDialog->fileName()))
	{
	    pref->setFilename(theFileSelectionDialog->fileName());
	    pref->loadPreferences();
	    info->getEntries()->reselect();
	}
	else
	{
	    sprintf(str, "Couldn't open '%s'", theFileSelectionDialog->fileName());
	    theWarningDialog->post(str);
	}
    }
}

// Write out the database and preferences when the user selects File...Save

void PhoneBook::saveMenuAction()
{
    theData->writeFile(pref->filename());
    pref->savePreferences();
}

// Create and show a file selection dialog when the user selects
// File...Save As.  If successful, change the current filename
// to the newly selected one.  Otherwise, post a warning.

void PhoneBook::saveAsMenuAction()
{
    char str[1024];
    
    theFileSelectionDialog->setTitle("Save Database");
    theFileSelectionDialog->setSelection(pref->filename());
    theFileSelectionDialog->postAndWait(NULL, "saveAsDialog", this);
    
    if (theFileSelectionDialog->fileName())
    {
	if (theData->writeFile((char *) theFileSelectionDialog->fileName()))
	{
	    pref->setFilename(theFileSelectionDialog->fileName());
	    pref->savePreferences();
	}
	else
	{
	    sprintf(str, "Couldn't save to '%s'",
		    theFileSelectionDialog->fileName());
	    theWarningDialog->post(str);
	}
    }
}

// Create and show a file selection dialog when the user selects
// File...Print.  Create a Print object as necessary, and try
// to print to the selected file.  If the print fails, display
// a warning.

void PhoneBook::printMenuAction()
{
    char str[1024];

    theFileSelectionDialog->setTitle("Print To File");
    theFileSelectionDialog->postAndWait(NULL, "printDialog", this);

    if (theFileSelectionDialog->fileName())
    {
	if (!print) 
	    print = new Print();

	if (!print->saveFile((char *) theFileSelectionDialog->fileName()))
	{
	    sprintf(str, "Couldn't print to '%s'",
		    theFileSelectionDialog->fileName());
	    theWarningDialog->post(str);
	}
    }
}

// Terminate the application when the user selects File...Exit.  Since
// VkApp::quitYourself uses the okToQuit protocol, we will have a chance
// to ask the user if they want to save any unsaved changes.

void PhoneBook::quitMenuAction()
{
    theApplication->quitYourself();
}

// X/Motif static callback stubs which call the appropriate class methods

void PhoneBook::newCallback(Widget, XtPointer clientData, XtPointer)
{
    PhoneBook *obj = (PhoneBook *) clientData;
    
    obj->newMenuAction();
}

void PhoneBook::prefCallback(Widget, XtPointer clientData, XtPointer)
{
    PhoneBook *obj = (PhoneBook *) clientData;
    
    obj->prefMenuAction();
}

void PhoneBook::loadCallback(Widget, XtPointer clientData, XtPointer)
{
    PhoneBook *obj = (PhoneBook *) clientData;
    
    obj->loadMenuAction();
}

void PhoneBook::saveCallback(Widget, XtPointer clientData, XtPointer)
{
    PhoneBook *obj = (PhoneBook *) clientData;
    
    obj->saveMenuAction();
}

void PhoneBook::saveasCallback(Widget, XtPointer clientData, XtPointer)
{
    PhoneBook *obj = (PhoneBook *) clientData;
    
    obj->saveAsMenuAction();
}

void PhoneBook::printCallback(Widget, XtPointer clientData, XtPointer)
{
    PhoneBook *obj = (PhoneBook *) clientData;
    
    obj->printMenuAction();
}

void PhoneBook::quitCallback(Widget, XtPointer clientData, XtPointer)
{
    PhoneBook *obj = (PhoneBook *) clientData;
    
    obj->quitMenuAction();
}
