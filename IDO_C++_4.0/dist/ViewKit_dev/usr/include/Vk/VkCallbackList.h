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
#ifndef VkCALLBACKLIST_H
#define VkCALLBACKLIST_H

#include <Vk/VkCallbackObject.h>

class VkCallbackItem {

  friend class VkCallbackList;
  friend class VkCallbackObject;

  public:

    VkCallbackItem(VkCallbackObject *obj, const char *const name, VkCallbackMethod, void * clientData);
    VkCallbackItem(const char *const name, VkCallbackFunction, void *clientData);
    ~VkCallbackItem();

    void invoke(VkCallbackObject *, const char * const, void *);

  private:

    VkCallbackObject   *_obj;
    char               *_name;
    VkCallbackFunction  _func;    
    VkCallbackMethod    _method;
    void               *_clientData;
};

class VkCallbackList {

 friend class VkCallbackObject;

  public:

  void add(VkCallbackItem *);
  void add(VkCallbackList *);
  void add(const char *const name,  VkCallbackObject *obj, VkCallbackMethod, void *clientData);
  void add(const char *const name, VkCallbackFunction, void *clientData);

  void invoke(VkCallbackObject *, const char *const, void *);
  int hasCallbacks(const char *);

  int size() { return (_numCallbacks);}

  VkCallbackItem *operator[] (int index) const { return (_callbacks[index]); }

  void remove(VkCallbackItem *);
  void remove(const char *const name, VkCallbackObject *obj, VkCallbackMethod, void *clientData);
  void remove(VkCallbackObject *obj);
  void remove (const char *const name, VkCallbackFunction, void *clientData);

  VkCallbackList();
  ~VkCallbackList();

  private:

    VkCallbackItem   **_callbacks;
    int                _numCallbacks;
};


#endif
