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

#ifndef VKPROMPTDIALOG_H
#define VKPROMPTDIALOG_H

#include <Vk/VkDialogManager.h>

class VkPromptDialog : public VkDialogManager {

  public:
    
    VkPromptDialog(const char* name) : VkDialogManager(name) { }
    ~VkPromptDialog();
    const char *text() { return _text; }

  protected:

    char *_text;
    virtual Widget createDialog(Widget);

    virtual Widget prepost ( const char      *message, 
			     XtCallbackProc   okCB       = NULL, 
			     XtCallbackProc   cancelCB   = NULL, 
			     XtCallbackProc   applyCB    = NULL, 
			     XtPointer        clientData = NULL,
			     const char      *helpString = NULL,
			     VkSimpleWindow  *parent     = NULL);    

    void ok(Widget, XtPointer);
};

extern VkPromptDialog *getThePromptDialog();
#define thePromptDialog getThePromptDialog()

#endif
