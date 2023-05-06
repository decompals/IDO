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
#ifndef VKPREFDIALOG
#define VKPREFDIALOG

#include <Vk/VkGenericDialog.h>

class VkPrefItem;

class VkPrefDialog : public VkGenericDialog {

  public:

    static const char *const prefCallback;

    VkPrefDialog(const char *name, VkPrefItem *item = NULL);
    virtual ~VkPrefDialog();

    VkPrefItem *item() { return _item; }
    void setItem(VkPrefItem *i) { _item = i; }

    virtual const char *className();

  protected:

    VkPrefItem *_item;
    virtual Widget createDialog(Widget parent);
    virtual void ok(Widget, XtPointer);
    virtual void cancel(Widget, XtPointer);
    virtual void apply(Widget, XtPointer);
    
};

#endif





