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
#ifndef VKLICENSE_H
#define VKLICENSE_H

#include <Vk/VkCallbackObject.h>
#include <Xm/Xm.h>

class VkLicense : public VkCallbackObject {

   public:

    // vndname:             vendor name
    // version:             version
    // interval_in_minutes: how many minutes apart check license

    VkLicense ( char *vndname,
		char *version,
		long interval_in_minutes = 10);

    ~VkLicense();

    virtual Boolean getLicense();
    virtual void giveUpLicense();
    virtual void showLicenseInfo();
    virtual void showLicenseUsers();


    static const char *const lostLicenseCallback;
    static const char *const licenseGoneCallback;
    static const char *const noLicenseCallback;

    enum ErrorCode {LICENSE_OK,
		    NO_LICENSE_SERVER,
		    NOT_ENOUGH_LICENSES,
		    NO_LICENSE,
		    NO_LICENSE_PASSWORD,
		    LICENSE_EXPIRED,
		    UNKNOWN_LICENSE_ERROR};

  protected:

    const char *getVendorName();
    const char *getVersion();
    long  getInterval();

    class  VkLicenseSupport  *_support;

    virtual ErrorCode   setupLicense(time_t&);
    virtual const char *getThirtyDayMessage(int);
    virtual const char *getSixtyDayMessage(int);
    virtual const char *getNinetyDayMessage(int);
    virtual Boolean     checkLicense();
    virtual void        licenseEstablished();
    virtual const char *getLicenseError();
    virtual const char *getAnnotation();

    virtual void installLicense(const char *);
    virtual Boolean licenseInstallerExists();

    void *extension;	// for future use

  private:

    static void checkLicenseCallback (XtPointer, XtIntervalId *);
};

#endif
