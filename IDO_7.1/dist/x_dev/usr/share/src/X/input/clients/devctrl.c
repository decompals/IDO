#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/extensions/XInput.h>
#include <X11/extensions/SGIMisc.h>
#include "IEUtils.h"

int
main(int argc, char *argv[])
{
    Display	*dpy;
    char	*progname;
    char	*device;
    char	*control;
    char	*value;
    XDevice	*pDev;
    int		tmp,id;

    if (argc<4) {
	progname = strrchr(argv[0], '/');
	if (progname == NULL)
	    progname = argv[0];
	else
	    progname += 1;
	if (strcmp(progname,"devquery")!=0) {
	    fprintf(stderr,"Usage: devctrl device control value [+]\n");
	    return -1;
        }
	else if (argc!=3) {
	    fprintf(stderr,"Usage: devquery device query\n");
	    return -1;
	}
    }
    device= argv[1];
    control= argv[2];
    if (argc>3) {
	value=  (char *)malloc(strlen(argv[3])+1);
	strcpy(value,argv[3]);
	if (argc>4)
	    strcat(value,"\015");
    }

    if ((dpy = XOpenDisplay(NULL)) == NULL) {
	fprintf(stderr, "Can't connect to server.\n");
	return -1;
    }
    XSynchronize(dpy,1);
    if (!IEUCheckExtension(dpy)) {
	fprintf(stderr, "Server doesn't support the input extension.\n");
	XCloseDisplay(dpy);
	return -1;
    }

    if (!XSGIMiscQueryExtension(dpy,&tmp,&tmp)) {
	fprintf(stderr, "Server doesn't support the SGI misc extension.\n");
	XCloseDisplay(dpy);
	return -1;
    }

    id= IEUFindDeviceIDByName(dpy, device);
    if (id<0) {
	fprintf(stderr, "Device \"%s\" not found.\n",device);
	XCloseDisplay(dpy);
	return -1;
    }
    if (argc>3) {
	XSGIDeviceControl(dpy,id,control,value);
    }
    else {
	char rtrn[SGIDeviceRtrnLen];

	/* this delay is here to allow the device module to */
	/* read configuration info from the device after it is opened. */
	sleep(1);
	if (!XSGIDeviceQuery(dpy,id,control,rtrn)) {
	    XCloseDisplay(dpy);
	    return -1;
	}
	else {
	    rtrn[SGIDeviceRtrnLen-1]= '\0';
	    printf("%s\n",rtrn);
	}
    }
    XCloseDisplay(dpy);
    return 0;
}
