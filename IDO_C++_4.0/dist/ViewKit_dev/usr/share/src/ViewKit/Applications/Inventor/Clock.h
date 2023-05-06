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
////////////////////////////////////////////////////
// Clock.h
////////////////////////////////////////////////////
#ifndef CLOCK_H
#define CLOCK_H

#include <Vk/VkComponent.h>
#include <Inventor/SbBasic.h>
#include <Inventor/nodes/SoGroup.h>
#include <Inventor/sensors/SoSensor.h>

class Clock : public VkComponent {
  public:
    Clock(const char *name, Widget parent);
    ~Clock();
    virtual const char *className();

  protected:
     SbBool setupHands(SoGroup *root);
     static void getCurrentTime(int &h, int &m, int &s);
};

#endif
