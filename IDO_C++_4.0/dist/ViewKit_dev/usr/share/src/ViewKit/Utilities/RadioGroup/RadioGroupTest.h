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
//////////////////////////////////////////////////////////////
// Header file for RadioGroupTest
//////////////////////////////////////////////////////////////

#ifndef _RADIOGROUPTEST_H
#define _RADIOGROUPTEST_H

#include <Vk/VkComponent.h>

class VkRadioGroup;

class RadioGroupTest : public VkComponent
{ 
  protected: 

    VkRadioGroup *_rg1, *_rg2;

    // Widgets created by this class

    Widget  _a1;
    Widget  _a2;
    Widget  _a3;
    Widget  _b1;
    Widget  _b2;
    Widget  _b3;
    Widget  _bulletinBoard;
    Widget  _text;

  public:

    RadioGroupTest(const char *, Widget);
    ~RadioGroupTest();
    const char* className();
};
#endif

