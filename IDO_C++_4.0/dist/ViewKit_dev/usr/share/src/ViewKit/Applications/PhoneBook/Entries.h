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
#ifndef ENTRIES_H
#define ENTRIES_H

#include <Vk/VkComponent.h>

class VkTabPanel;
class Info;
class Entry;
class Entries;
class VkPopupMenu;

class Entries : public VkComponent {
  public:
    Entries(const char *name, Widget parent, Info *info);
    ~Entries();
    
    const char *className();

    void display(char *range);
    void reselect();
    void postMenu(Entry *entry, XEvent *event);

    XmFontList normalFont() { return buttonFont; }

  protected:
    void deleteAll();
    void addDisplay(char letter);
    Entry *addEntry(const char *name);
    void editMenu();
    void deleteMenu();

    static void editCallback(Widget w, XtPointer clientData, XtPointer callData);
    static void deleteCallback(Widget w, XtPointer clientData, XtPointer callData);

    Info *info;
    int numEntries, sizeEntries, realEntries;
    Entry **entries, *menuEntry;
    VkPopupMenu *entryPopup;
    XmFontList buttonFont, labelFont;
    char *lastRange;
};

#endif
