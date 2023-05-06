/*
 * vlInit.c: Does libvl (video library) initialization
 *
 * 
 * Silicon Graphics Inc., June 1994
 */
#include "dmplay.h"
#include <vl/dev_ev1.h>


/*
 *vlInit: Does initial vl settings.
 *
 */
void vlInit ()
{
    VLDevice *devPtr;
    VLDevList devList;
    VLControlValue val;
    int n;

    if (image.display == GRAPHICS) 
	return;
    
    if (!(video.svr = vlOpenVideo("")) ) {
        vlPerror("Unable to open video -- ");
        exit(1);
    }
    
    vlGetDeviceList(video.svr, &devList);
    
    for (n = 0; n < devList.numDevices; n++) {
	devPtr = &(devList.devices[n]);
        if (strcmp (devPtr->name, "ev1") == 0)
	    break;
        else devPtr = NULL;
    }
    
    if (devPtr == NULL) {
	fprintf(stderr, "IndyVideo/Galileo not found\n");
	exit (1);
    }

    video.devNode = vlGetNode(video.svr, VL_DEVICE, 0, VL_ANY);

    video.path = vlCreatePath(video.svr, devPtr->dev, video.devNode, video.devNode);
    if (video.path == -1) vlPerror("Bad path");

    vlSetupPaths(video.svr, (VLPathList)&video.path, 1, VL_SHARE, VL_READ_ONLY);

    vlGetControl(video.svr, video.path, video.devNode, VL_SYNC, &val);
    if (val.intVal == VL_EV1_SYNC_SLAVE) {
	val.intVal = VL_SYNC_INTERNAL;
	if (vlSetControl(video.svr, video.path, video.devNode, VL_SYNC, &val))
	    vlPerror("Warning: unable to set video timing -- ");
    }

    vlDestroyPath(video.svr, video.path);
    
    /*
    ** setup the timing path
    ** (IndyVideo/Galileo -> Cosmo)
    */
    video.timingSrc = vlGetNode(video.svr, VL_SRC, VL_SCREEN, VL_ANY);
    video.timingDrn = vlGetNode(video.svr, VL_DRN, VL_VIDEO, 2);
    video.timingPath = 
	vlCreatePath(video.svr, devPtr->dev, video.timingSrc, video.timingDrn); 
    if (video.timingPath == -1) {
	vlPerror("vlCreatePath failed for timing path");
	exit(1);
    }

    if (vlSetupPaths(video.svr, (VLPathList)&video.timingPath, 1, 
						VL_SHARE, VL_SHARE) < 0) {
	vlPerror ("Unable to start video timing -- ");
	exit (1);
    }

    if (vlBeginTransfer(video.svr, video.timingPath, 0, NULL)) {
	vlPerror ("Unable to vlBeginTransfer video timing -- ");
	exit (1);
    }
    video.timingActive = 1;

    /*
    ** setup the data transfer path
    ** (Cosmo -> IndyVideo/Galileo --> screen, video)
    */
    video.src = vlGetNode(video.svr, VL_SRC, VL_VIDEO, 1);
    video.drn = vlGetNode(video.svr, VL_DRN, VL_SCREEN, VL_ANY);
    video.voutdrn = vlGetNode(video.svr, VL_DRN, VL_VIDEO, 0);
    video.dataTiming = vlGetNode(video.svr, VL_DRN, VL_VIDEO, 2);
    
    video.path = vlCreatePath(video.svr, devPtr->dev, video.src, video.drn); 
    if (video.path == -1) {
	vlPerror("vlCreatePath failed for data path");
	vlDestroyPath(video.svr, video.timingPath);
	exit(1);
    }

    if (vlAddNode(video.svr, video.path, video.voutdrn)) {
	vlPerror("voutdrn Node Error");
    }

    if (vlAddNode(video.svr, video.path, video.dataTiming)) {
	vlPerror("dataTiming Node Error");
    }

    if (vlSetupPaths(video.svr, (VLPathList)&video.path, 1,
						 VL_SHARE, VL_SHARE) < 0) {
	vlPerror ("Unable to setup data path");
	vlDestroyPath(video.svr, video.timingPath);
	exit(1);
    }
}
