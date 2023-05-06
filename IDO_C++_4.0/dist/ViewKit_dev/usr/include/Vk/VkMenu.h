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

#ifndef VKMENU_H
#define VKMENU_H

#include <Vk/VkMenuItem.h>

class VkWindow;
class VkMenuAction;
class VkMenuActionWidget;
class VkMenuToggle;
class VkSubMenu;
class VkRadioSubMenu;

struct VkMenuDesc {
    VkMenuItemType       menuType;
    char		*name;    
    XtCallbackProc       callback;
    VkMenuDesc          *submenu;
    XtPointer            clientData;
    XtCallbackProc       undoCallback;
};

class VkMenu : public VkMenuItem {

    friend   VkMenuItem;
    friend   VkMenuBar;
    friend   VkSubMenu;

  public:

    virtual ~VkMenu();

    virtual void build(Widget);

    VkMenuAction       *addAction(const char     *name, 
				  XtCallbackProc  func = NULL, 
				  XtPointer       data      = NULL, 
				  int             pos = -1);
    VkMenuAction       *addAction(const char     *name,
				  XtCallbackProc  func, 
				  XtCallbackProc  undoCallback, 
				  XtPointer       data, 
				  int             pos  = -1);
    VkMenuActionWidget *addActionWidget(const char     *name, 
					XtCallbackProc func = NULL, 
					XtPointer      data = NULL, 
					int            pos = -1);
    VkMenuActionWidget *addActionWidget(const char      *name, 
					XtCallbackProc   func,
					XtCallbackProc   undoCallback, 
					XtPointer        data, 
					int              pos = -1);

    VkMenuConfirmFirstAction  *addConfirmFirstAction(const char     *name, 
						     XtCallbackProc  func = NULL, 
						     XtPointer       data = NULL, 
						     int             pos = -1);

    VkMenuSeparator    *addSeparator(int pos = -1);

    VkMenuLabel        *addLabel(const char *name, int pos = -1);

    VkMenuToggle       *addToggle(const char     *name, 
				  XtCallbackProc  func = NULL, 
				  XtPointer       data = NULL, 
				  int             state = -1, 
				  int             pos = -1);
    VkMenuToggle       *addToggle(const char     *name, 
				  XtCallbackProc  func, 
				  XtCallbackProc  undoCallback, 
				  XtPointer       data, 
				  int             state = -1, 
				  int             pos = -1);

    void                add(VkMenuItem *item, int pos = -1);

    VkSubMenu *addSubmenu(VkSubMenu *submenu, int pos = -1);
    VkSubMenu *addSubmenu(const char *name, int pos = -1);
    VkSubMenu *addSubmenu(const char *name, VkMenuDesc*, XtPointer defaultClientData = NULL, int pos = -1);

    VkRadioSubMenu *addRadioSubmenu(VkRadioSubMenu *submenu, int pos = -1);
    VkRadioSubMenu *addRadioSubmenu(const char *name, int pos = -1);
    VkRadioSubMenu *addRadioSubmenu(const char *name, VkMenuDesc*, XtPointer defaultClientData = NULL);

    // enter a submenu into the system, but don't show or attach it
    // This allows it to be found using findNamedItem
    
    VkMenuItem *registerSubmenu(const char * name, XtCallbackProc func, XtPointer data, VkMenuItem * submenu);
    
    // manipulation of existing menu items, many more to be added

    VkMenuItem *findNamedItem(const char * , Boolean caseless = FALSE);

    VkMenuItem *removeItem(const char *);
    VkMenuItem *activateItem(const char * );
    VkMenuItem *deactivateItem(const char * );
    VkMenuItem *replace(const char * , VkMenuItem * );

    static  void useWorkProcs(Boolean flag);
    virtual VkMenuItemType menuType () = 0;
    virtual const char* className();
    Boolean isContainer();
    int getItemPosition(VkMenuItem*);
    int getItemPosition(char *name);
    int getItemPosition(Widget);
    VkMenuItem * operator[] (int index) const;
    int numItems() const;

  protected:

    VkMenuItem     **_contents;
    int              _nItems;
    int              _maxItems;

    VkMenu();
    VkMenu(const char *name);
    VkMenu(VkMenuDesc *, XtPointer defaultClientData=NULL);
    VkMenu(const char *name, VkMenuDesc *, XtPointer defaultClientData=NULL);

    VkMenu *findParent(VkMenuItem *);
    virtual Widget menuParent() = 0;

  private:

    static Boolean _useWorkProcs;
    void handleMenuDescriptor(VkMenuDesc *, XtPointer);
    void addMenuEntry(VkMenuItem *, int pos = -1);
    int  removeMenuEntry(VkMenuItem *);

    
};




#endif




