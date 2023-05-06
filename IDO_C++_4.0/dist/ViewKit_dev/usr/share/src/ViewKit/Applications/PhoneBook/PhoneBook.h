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
#ifndef PHONE_BOOK
#define PHONE_BOOK

#include <Vk/VkWindow.h>

class Info;
class EntryDialog;
class Preferences;
class Print;
class PrefDialog;

class PhoneBook : public VkWindow {
  public:
    PhoneBook(const char *name);
    ~PhoneBook();

    virtual const char *className();
    void prefChanged();

  protected:
    virtual void handleWmDeleteMessage();
    virtual Boolean okToQuit();
    void addFileMenu();
    
    void newMenuAction();
    void prefMenuAction();
    void loadMenuAction();
    void saveMenuAction();
    void saveAsMenuAction();
    void printMenuAction();
    void quitMenuAction();

    static void newCallback(Widget w, XtPointer clientData, XtPointer callData);
    static void prefCallback(Widget w, XtPointer clientData, XtPointer callData);
    static void loadCallback(Widget w, XtPointer clientData, XtPointer callData);
    static void saveCallback(Widget w, XtPointer clientData, XtPointer callData);
    static void saveasCallback(Widget w, XtPointer clientData, XtPointer callData);
    static void printCallback(Widget w, XtPointer clientData, XtPointer callData);
    static void quitCallback(Widget w, XtPointer clientData, XtPointer callData);
    
    Info *info;
    EntryDialog *newDialog;
    Preferences *pref;
    Print *print;
    PrefDialog *prefDialog;
};

#endif
