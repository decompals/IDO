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
#ifndef VKCOMPLETIONFIELD_H
#define VKCOMPLETIONFIELD_H

#include <Vk/VkComponent.h>

class VkNameList;

class VkCompletionField : public VkComponent {

  public:

    static const char * const enterCallback;
    
    VkCompletionField(const char *name, Widget);
    ~VkCompletionField();

    void add(char* name);
    void clear(VkNameList *nameList = NULL);	// existing list deleted


    char *getText();	
    virtual const char* className();

  private:

    static void expandCallback(Widget, XtPointer, XtPointer);
    static void activateCallback(Widget, XtPointer, XtPointer);

  protected:

    VkNameList *_currentMatchList;
    VkNameList   *_nameList;

    virtual void expand(struct XmTextVerifyCallbackStruct *);
    virtual void activate(struct XmTextVerifyCallbackStruct *);
}; 

#endif
