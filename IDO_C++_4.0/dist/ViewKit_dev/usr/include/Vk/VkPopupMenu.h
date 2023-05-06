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

#ifndef VKPOPUPMENU_H
#define VKPOPUPMENU_H

#include <Vk/VkMenu.h>
#include <Vk/VkWidgetList.h>

class VkPopupMenu : public VkMenu {


  public:

    VkPopupMenu(VkMenuDesc *desc, XtPointer defaultClientData = NULL);
    VkPopupMenu(const char *name = "popupMenu", VkMenuDesc *desc = NULL, XtPointer defaultClientData = NULL);

    VkPopupMenu(Widget, VkMenuDesc *desc, XtPointer defaultClientData = NULL);
    VkPopupMenu(Widget, const char *name = "popupMenu", VkMenuDesc *desc = NULL, XtPointer defaultClientData = NULL);

    ~VkPopupMenu();

    virtual VkMenuItemType menuType ();

    virtual void build(Widget p);
    virtual void attach(Widget p);
    virtual void show();
    virtual void show(XEvent *);
    virtual const char* className();

  protected:

    Widget menuParent();
    VkWidgetList _attachees;
    virtual void widgetDestroyed();
    
  private:

    static void postMenuHandler( Widget,  XtPointer , XEvent *, Boolean *);
    
};
#endif