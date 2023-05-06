#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/extensions/XInput.h>
#include <X11/keysym.h>
#include "IEUtils.h"
#include <X11/extensions/SGIMisc.h>

int
main(int argc, char *argv[])
{
    Display	*dpy;
    XDevice	*pDev;
    char	*device;
    int		i, id, ndevs;
    int		do_setptr = 0;
    char	rtrn[SGIDeviceRtrnLen];
    XDeviceInfoPtr listSav, list, oldPtrDev = NULL, newPtrDev = NULL;

    if (strcmp(argv[0],"setptr")==0)
	do_setptr = 1;

    if (argc>1)
	device= argv[1];
    else {
	fprintf(stderr,"Usage: %s device_name\n",argv[0]);
	return(-1);
    }

    if ((dpy=XOpenDisplay(NULL)) == NULL) {
	fprintf(stderr, "Couldn't connect to server.\n");
	return(-1);
    }
    XSynchronize(dpy,1);
    if (!IEUCheckExtension(dpy)) {
	return(-1);
    }

    if (do_setptr) {
	/*
	 * Find old ptr device.
	 */
	listSav = (XDeviceInfoPtr) XListInputDevices (dpy, &ndevs);
	for (list = listSav, i = 0; i < ndevs; i++, list++) {
	    if (list->use == IsXPointer
		&& strcmp(list->name, "virtual_pointer") != 0) {
		    oldPtrDev = list;
		    break;
	    } else if (list->use == IsXExtensionDevice &&
		XSGIDeviceQuery(dpy, list->id, "pushpointer", rtrn) &&
		(strcmp(rtrn, "on") == 0 || strcmp(rtrn, "exclusive") == 0)) {
		    oldPtrDev = list;
		    break;
	    }
	}
	/*
	 * Now find ptr to _XDeviceInfo struct corresponding
	 *  to new ptr device.
	 */
	if (strcmp(device,"none") != 0) {
	    for (list = listSav, i = 0; i < ndevs; i++, list++) {
		int use = list->use;
		if (use == IsXExtensionDevice || use == IsXPointer) {
		    if (strcmp(list->name, device) == 0) {
			newPtrDev = list;
			break;
		    }
		}
	    }
	}
	if (newPtrDev != oldPtrDev) {
	    if (oldPtrDev)
		XSGIDeviceControl(dpy, oldPtrDev->id, "pushpointer", "off");
	    if (newPtrDev) {
		XSGIDeviceControl(dpy, newPtrDev->id, "autostart", "on");
		XSGIDeviceControl(dpy, newPtrDev->id, "pushpointer", "on");
	    }
	}
    } else {
	/* do_setptr == false */
	pDev= IEUOpenDeviceByName(dpy, device);
	if (pDev==NULL) {
	    fprintf(stderr, "Device \"%s\" not found.\n",device);
	    return(-1);
	}
	XChangeKeyboardDevice(dpy, pDev);
    }
    XCloseDisplay(dpy);
    return 0;
}

