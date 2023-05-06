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

#ifndef VKWINDOW_H
#define VKWINDOW_H

// VkWindow.h - declarations for a Motif toplevel window object that supports a menu

class VkApp;
class VkSubMenu;
class VkRadioSubMenu;
class VkMenuBar;
struct VkMenuDesc;

#include <Vk/VkSimpleWindow.h>

// VkWindow class

class VkWindow : public VkSimpleWindow {

  public:

    VkWindow(const char *name, ArgList argList = NULL, Cardinal argCount = NULL);    
    virtual ~VkWindow();

    virtual void show();    

    void    setMenuBar(VkMenuBar *menuObj);
    void    setMenuBar(VkMenuDesc *menudesc);

    VkSubMenu *addMenuPane(const char *);
    VkSubMenu *addMenuPane(const char *, VkMenuDesc *);

    VkRadioSubMenu *addRadioMenuPane(const char *);
    VkRadioSubMenu *addRadioMenuPane(const char *, VkMenuDesc *);

    virtual VkMenuBar *menu() const;
    virtual const char* className();

  protected:

    virtual Widget setUpInterface(Widget );
    
  private:

    VkMenuBar *_menu;
}; 
#endif
