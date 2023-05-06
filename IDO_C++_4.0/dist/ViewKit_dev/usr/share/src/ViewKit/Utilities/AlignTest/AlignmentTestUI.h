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
//
// Header file for AlignmentTestUI
//
//////////////////////////////////////////////////////////////
#ifndef _AlignmentTestUI_H
#define _AlignmentTestUI_H
#include <Vk/VkComponent.h>


class AlignmentTestUI : public VkComponent
{ 

  private: 

    // Array that describes interactions with Xt resource manager

    static String      _defaultAlignmentTestUIResources[];


  protected: 

    // Widgets created by this class

    Widget  _a;
    Widget  _aa;
    Widget  _alalalalalalalalalala;
    Widget  _bulletinBoard;
    Widget  _long;
    Widget  _longer;
    Widget  _pushButton;
    Widget  _pushButton1;
    Widget  _pushButton2;
    Widget  _pushButton3;
    Widget  _qwerty;
    Widget  _short;
    Widget  _veryverylong;

  public:

    AlignmentTestUI(const char *, Widget);
    ~AlignmentTestUI();
    const char* className();
};
#endif

