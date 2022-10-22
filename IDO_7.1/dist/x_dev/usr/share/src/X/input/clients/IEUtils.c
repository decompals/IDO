#include <stdio.h>
#define XLIB_ILLEGAL_ACCESS
#include <X11/Xlib.h>
#include <X11/extensions/XInput.h>

/***====================================================================***/

int		IEUKeyPressType;
XEventClass	IEUKeyPressClass;
int		IEUKeyReleaseType;
XEventClass	IEUKeyReleaseClass;
int		IEUButtonPressType;
XEventClass	IEUButtonPressClass;
int		IEUButtonReleaseType;
XEventClass	IEUButtonReleaseClass;
int		IEUMotionNotifyType;
XEventClass	IEUMotionNotifyClass;
int		IEUFocusInType;
XEventClass	IEUFocusInClass;
int		IEUFocusOutType;
XEventClass	IEUFocusOutClass;
int		IEUProximityInType;
XEventClass	IEUProximityInClass;
int		IEUProximityOutType;
XEventClass	IEUProximityOutClass;
int		IEUStateNotifyType;
XEventClass	IEUStateNotifyClass;
int		IEUMappingNotifyType;
XEventClass	IEUMappingNotifyClass;
int		IEUChangeNotifyType;
XEventClass	IEUChangeNotifyClass;
int		IEUPointerMotionHintType;
XEventClass	IEUPointerMotionHintClass;
int		IEUButton1MotionType;
XEventClass	IEUButton1MotionClass;
int		IEUButton2MotionType;
XEventClass	IEUButton2MotionClass;
int		IEUButton3MotionType;
XEventClass	IEUButton3MotionClass;
int		IEUButton4MotionType;
XEventClass	IEUButton4MotionClass;
int		IEUButton5MotionType;
XEventClass	IEUButton5MotionClass;
int		IEUOwnerGrabButtonType;
XEventClass	IEUOwnerGrabButtonClass;
int		IEUButtonPressGrabType;
XEventClass	IEUButtonPressGrabClass;

/***====================================================================***/

int
IEUCheckExtension(Display *dpy)
{
int	ieMajor,ieFEV,ieFER;

    if (!XQueryExtension(dpy, INAME, &ieMajor, &ieFEV, &ieFER)) {
	fprintf(stderr, "Server on \"%s\" does not support %s.\n",
						dpy->display_name, INAME);
	return 0;
    };
    return 1;
}

/***====================================================================***/

int
IEUFindDeviceIDByName(Display *dpy,char *name)
{
XDeviceInfo	*pDevInfo,*pOrig;
int		 nDev,i;
int		 devID;

    pDevInfo= pOrig= XListInputDevices(dpy, &nDev);
    devID= -1;
    for (i= 0; i < nDev; i++, pDevInfo++) {
	if (strcmp(pDevInfo->name, name) == 0) {
	    devID= pDevInfo->id;
	    break;
	}
    }
    XFreeDeviceList(pOrig); 
    return devID;
}

/***====================================================================***/

static void
IEUAddDeviceEventClasses(XDevice *pDev)
{
    if (IEUKeyPressType==0)
	DeviceKeyPress(pDev,IEUKeyPressType,IEUKeyPressClass);
    if (IEUKeyReleaseType==0)
	DeviceKeyRelease(pDev,IEUKeyReleaseType,IEUKeyReleaseClass);
    if (IEUButtonPressType==0)
	DeviceButtonPress(pDev,IEUButtonPressType,IEUButtonPressClass);
    if (IEUButtonReleaseType==0)
	DeviceButtonRelease(pDev,IEUButtonReleaseType,IEUButtonReleaseClass);
    if (IEUMotionNotifyType==0)
	DeviceMotionNotify(pDev,IEUMotionNotifyType,IEUMotionNotifyClass);
    if (IEUFocusInType==0)
	DeviceFocusIn(pDev,IEUFocusInType,IEUFocusInClass);
    if (IEUFocusOutType==0)
	DeviceFocusOut(pDev,IEUFocusOutType,IEUFocusOutClass);
    if (IEUProximityInType==0)
	ProximityIn(pDev,IEUProximityInType,IEUProximityInClass);
    if (IEUProximityOutType==0)
	ProximityOut(pDev,IEUProximityOutType,IEUProximityOutClass);
    if (IEUStateNotifyType==0)	
	DeviceStateNotify(pDev,IEUStateNotifyType,IEUStateNotifyClass);
    if (IEUMappingNotifyType==0)
	DeviceMappingNotify(pDev,IEUMappingNotifyType,IEUMappingNotifyClass);
    if (IEUChangeNotifyType==0)
	ChangeDeviceNotify(pDev,IEUChangeNotifyType,IEUChangeNotifyClass);
    if (IEUPointerMotionHintType==0)
	DevicePointerMotionHint(pDev,IEUPointerMotionHintType,
					IEUPointerMotionHintClass);
    if (IEUButton1MotionType==0)
	DeviceButton1Motion(pDev,IEUButton1MotionType,IEUButton1MotionClass);
    if (IEUButton2MotionType==0)
	DeviceButton2Motion(pDev,IEUButton2MotionType,IEUButton2MotionClass);
    if (IEUButton3MotionType==0)
	DeviceButton3Motion(pDev,IEUButton3MotionType,IEUButton3MotionClass);
    if (IEUButton4MotionType==0)
	DeviceButton4Motion(pDev,IEUButton4MotionType,IEUButton4MotionClass);
    if (IEUButton5MotionType==0)
	DeviceButton5Motion(pDev,IEUButton5MotionType,IEUButton5MotionClass);
    if (IEUOwnerGrabButtonType==0)
	DeviceOwnerGrabButton(pDev,IEUOwnerGrabButtonType,
					IEUOwnerGrabButtonClass);
    if (IEUButtonPressGrabType==0)
	DeviceButtonPressGrab(pDev,IEUButtonPressGrabType,
					IEUButtonPressGrabClass);
    return;
}

/***====================================================================***/

XDevice	*
IEUOpenDeviceByName(Display *dpy,char *name)
{
XDeviceInfo	*pDevInfo,*pOrig;
int		 nDev,i;
XDevice		*pDev;

    pDevInfo = pOrig = XListInputDevices(dpy, &nDev);
    pDev= NULL;
    for (i = 0; i < nDev; i++, pDevInfo++) {
	if (strcmp(pDevInfo->name, name) == 0) {
	    pDev= XOpenDevice(dpy, pDevInfo->id);
	    break;
	}
    }
    XFreeDeviceList(pOrig); 
    if (pDev!=NULL)
	IEUAddDeviceEventClasses(pDev);
    return pDev;
}

/***====================================================================***/

XDevice	*
IEUOpenDeviceByID(Display *dpy,int id)
{
XDevice	*pDev;

    pDev= XOpenDevice(dpy, id);
    if (pDev!=NULL)
	IEUAddDeviceEventClasses(pDev);
    return pDev;
}
