#ifndef IEUTILS_H
#define IEUTILS_H 1

extern	int		IEUKeyPressType;
extern	XEventClass	IEUKeyPressClass;
extern	int		IEUKeyReleaseType;
extern	XEventClass	IEUKeyReleaseClass;
extern	int		IEUButtonPressType;
extern	XEventClass	IEUButtonPressClass;
extern	int		IEUButtonReleaseType;
extern	XEventClass	IEUButtonReleaseClass;
extern	int		IEUMotionNotifyType;
extern	XEventClass	IEUMotionNotifyClass;
extern	int		IEUFocusInType;
extern	XEventClass	IEUFocusInClass;
extern	int		IEUFocusOutType;
extern	XEventClass	IEUFocusOutClass;
extern	int		IEUProximityInType;
extern	XEventClass	IEUProximityInClass;
extern	int		IEUProximityOutType;
extern	XEventClass	IEUProximityOutClass;
extern	int		IEUStateNotifyType;
extern	XEventClass	IEUStateNotifyClass;
extern	int		IEUMappingNotifyType;
extern	XEventClass	IEUMappingNotifyClass;
extern	int		IEUChangeNotifyType;
extern	XEventClass	IEUChangeNotifyClass;
extern	int		IEUPointerMotionHintType;
extern	XEventClass	IEUPointerMotionHintClass;
extern	int		IEUButton1MotionType;
extern	XEventClass	IEUButton1MotionClass;
extern	int		IEUButton2MotionType;
extern	XEventClass	IEUButton2MotionClass;
extern	int		IEUButton3MotionType;
extern	XEventClass	IEUButton3MotionClass;
extern	int		IEUButton4MotionType;
extern	XEventClass	IEUButton4MotionClass;
extern	int		IEUButton5MotionType;
extern	XEventClass	IEUButton5MotionClass;
extern	int		IEUOwnerGrabButtonType;
extern	XEventClass	IEUOwnerGrabButtonClass;
extern	int		IEUButtonPressGrabType;
extern	XEventClass	IEUButtonPressGrabClass;

extern	int	 IEUCheckExtension(Display *dpy);
extern	int	 IEUFindDeviceIDByName(Display *dpy,char *name);
extern	XDevice	*IEUOpenDeviceByName(Display *dpy,char *name);
extern	XDevice	*IEUOpenDeviceByID(Display *dpy,int id);

#endif /* IEUTILS_H */
