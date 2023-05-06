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

#ifndef VKFILESELECTIONDIALOG_H
#define VKFILESELECTIONDIALOG_H

/////////////////////////////////////////////////////////////////////////
// VkFileSelectionDialog.h --
//   hajadi@csd.sgi.com
/////////////////////////////////////////////////////////////////////////

#include <Vk/VkDialogManager.h>

class VkFileSelectionDialog : public VkDialogManager {

 public:

    VkFileSelectionDialog( const char* name ) : VkDialogManager( name ) { _fileName   = NULL; 
									  _selection  = NULL;
									  _filter     = NULL;
									  _directory  = NULL;  
									  _showApply  = TRUE;  }
    virtual ~VkFileSelectionDialog();

    const char* className();
    const char* fileName()       const { return _fileName; }

    // Interface

    void setDirectory(const char *);
    void setSelection(const char *);
    void setFilterPattern(const char *);

  protected:

    virtual Widget createDialog( Widget dialogParent );

    virtual void ok(Widget, XtPointer);
    virtual void apply(Widget, XtPointer);
    virtual void cancel(Widget, XtPointer);

    XmFileSelectionBoxCallbackStruct *callData() const;


  private:

    char  *_fileName;
    char *_selection;
    char *_filter;
    char *_directory;

    XmFileSelectionBoxCallbackStruct *_callData;
};

extern VkFileSelectionDialog *getTheFileSelectionDialog();
    
#define theFileSelectionDialog getTheFileSelectionDialog()


#endif // VKFILESELECTIONDIALOG_H
