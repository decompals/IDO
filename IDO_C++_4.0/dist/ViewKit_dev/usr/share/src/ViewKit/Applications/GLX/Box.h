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
///////////////////////////////////////////////////////////////////////////
// Box.h
//////////////////////////////////////////////////////////////////////////
#ifndef BOX_H
#define BOX_H

#include "VkGLX.h"

class Box;

typedef struct {
    Box *obj;
    char axis;
    XtIntervalId id;
} RotateInfo;

class Box : public VkGLX {

  public:
     Box(const char *name, Widget parent);
     ~Box();

  protected:
    
     virtual void ginit(GlxDrawCallbackStruct *cb);
     virtual void expose(GlxDrawCallbackStruct *cb);
     virtual void resize(GlxDrawCallbackStruct *cb);
     virtual void input(GlxDrawCallbackStruct *cb);

     void initialize_gl();
     void viewcube(char axis);
     void updatemat(char axis);
     float norm_dot(Coord passpoly[][3]);
     void transform(long n, Coord passpoly[][3], Coord postrans[][3]);
     void resetAction();
     void translateAction(char axis, float amount);
     void rotateAction(char axis, int amount);

     static void rotateCallback(XtPointer clientData, XtIntervalId *id);

     Coord _x, _y, _z;
     Angle _rx, _ry, _rz;
     RotateInfo _rotX, _rotY, _rotZ;
};

#endif
