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
#ifndef _PREFERENCES_
#define _PREFERENCES_

#include <X11/Intrinsic.h>

#define DEFAULT_FILENAME "~/.phone"

class Preferences;

extern Preferences *thePreferences;

class Preferences {
  public:
    Preferences(Widget w);
    ~Preferences();
    
    Boolean isDirty() { return dirty; }
    
    char *filename() { return _filename; }
    Boolean showAddr() { return _showAddr; }
    Boolean showComment() { return _showComment; }
    int itemSpacing() { return _itemSpacing; }
    int printWidth() { return _printWidth; }
    
    void setFilename(const char *v);
    void setShowAddr(Boolean v) { _showAddr = v; dirty = True; }
    void setShowComment(Boolean v) { _showComment = v; dirty = True; }
    void setItemSpacing(int v) { _itemSpacing = v; dirty = True; }
    void setPrintWidth(int v) { _printWidth = v; dirty = True; }
    
    void loadPreferences();
    void savePreferences();
    
  protected:
    int readStr(FILE *fd, char *str_return);
    int readInt(FILE *fd, int *num);
    
    Boolean dirty;
    char *_filename;
    Boolean _showAddr;
    Boolean _showComment;
    int _itemSpacing;
    int _printWidth;
};

#endif
