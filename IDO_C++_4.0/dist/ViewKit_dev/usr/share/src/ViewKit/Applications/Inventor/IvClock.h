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
///////////////////////////////////////////////////////////////
// IvClock.h
/////////////////////////////////////////////////////////////
#ifndef IVCLOCK_H
#define IVCLOCK_H

#include <Vk/VkWindow.h>

class Clock;

class IvClock : public VkWindow {

  public:
    IvClock(const char *name);
    ~IvClock();

    virtual const char *className();

  protected:
     static void quitCallback(Widget w, XtPointer clientData, XtPointer callData);

     Clock *_clock;
};

#endif