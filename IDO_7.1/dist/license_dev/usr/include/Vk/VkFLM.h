////////////////////////////////////////////////////////////////////////////////
///////   Copyright 1996, Silicon Graphics, Inc.  All Rights Reserved.   ///////
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
#ifndef VKFLM_H
#define VKFLM_H

#include <Vk/VkLicense.h>

#include <Xm/Xm.h>

#include <lmsgi.h>

class VkFLM : public VkLicense{

  public:

    //
    // vndname:             vendor name
    // feature:             feature name
    // version:             version
    // interval_in_minutes: how many minutes apart to call license_timer()

    VkFLM ( VENDORCODE *code,
	    char *vndname,
	    char *feature,
	    char *version,
	    long interval_in_minutes = 10);

    ~VkFLM();

    virtual void giveUpLicense();
    virtual void showLicenseInfo();
    virtual void showLicenseUsers();

  protected:

    //
    // callbacks for losing a connection and re-establishing one
    //

    ErrorCode   setupLicense(time_t&);
    Boolean     checkLicense();
    void        licenseEstablished();
    const char *getLicenseError();
    const char *getAnnotation();

    virtual void installLicense(const char *);
    virtual Boolean licenseInstallerExists();

  private:

    VENDORCODE *_code;
    char       *_feature;

    static void reconnect_attempt (char *feature,
				   int pass, int total, int interval);
    static void reconnect_done    (char *feature,
				   int pass, int total, int interval);
    static void exitcall          (char *feature);
};

#ifndef VIEWKIT_BUILD

// Only activate this macro in user's code, so symbol
// doesn\'t appear in ViewKit library

LM_CODE(VkFLM_licenseCode,
	 ENCRYPTION_CODE_1, ENCRYPTION_CODE_2, VENDOR_KEY1,
         VENDOR_KEY2, VENDOR_KEY3, VENDOR_KEY4, VENDOR_KEY5);

#endif

#endif
