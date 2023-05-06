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


#ifndef VKCALLBACKOBJECT_H
#define VKCALLBACKOBJECT_H

class VkCallbackObject;
class VkCallbackObjectList;
class VkCallbackList;

typedef void (*VkCallbackFunction) (VkCallbackObject *caller, void *clientData, void *callData);
typedef void (VkCallbackObject::*VkCallbackMethod) (VkCallbackObject *caller, void *clientData, void *callData);

#define VkAddCallbackFunction(name, you, func, clientData) ( (you)->addCallback(name, func, clientData))
#define VkAddCallbackMethod(name, you, me, func, clientData) ( (you)->addCallback(name, (me), (VkCallbackMethod) func, clientData))

#define VkRemoveCallbackFunction(name, you, func, clientData) ( (you)->removeCallback(name, func, clientData))
#define VkRemoveCallbackMethod(name, you, me, func, clientData) ( (you)->removeCallback(name, (me), (VkCallbackMethod) func, clientData))

class VkCallbackObject {

 public:

    virtual ~VkCallbackObject();
    virtual const char *className();

    void addCallback(const char *, 
		     VkCallbackFunction, 
		     void *clientData = 0);
    void addCallback(const char *, 
		     VkCallbackObject *, 
		     VkCallbackMethod, 
		     void *clientData = 0);

    void removeCallback(const char *, 
			VkCallbackFunction, 
			void *clientData = 0);
    void removeCallback(const char *, 
			VkCallbackObject *, 
			VkCallbackMethod,
			void *clientData = 0);
    void removeAllCallbacks();
    void removeAllCallbacks(VkCallbackObject *);
    int hasCallbacks(const char *) const;
    void cloneCallbacks(VkCallbackObject *);
    void cloneCallback(const char * const, VkCallbackObject *);
    void cloneCallback(const char * const, const char * const,
		       VkCallbackObject *);

  protected:

    void callCallbacks(const char *const, void *);
    VkCallbackObject( );     // Protected constructor forces subclasses to redefine

  private:

    VkCallbackList        *_callbacks;
    VkCallbackObjectList  *_senders;

    void registerSender(VkCallbackObject *); 
    void unregisterSender(VkCallbackObject *); 
    void unregisterSenders(VkCallbackObject *); 

    
};

#endif

