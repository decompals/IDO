#ifndef _VL_H_
#define _VL_H_

/*
 *  vl.h -- Main header file for the IRIS Video Library.
 *
 * Copyright 1993,1994 Silicon Graphics, Inc.
 * All Rights Reserved.
 *
 * This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
 * the contents of this file may not be disclosed to third parties, copied or
 * duplicated in any form, in whole or in part, without the prior written
 * permission of Silicon Graphics, Inc.
 *
 * RESTRICTED RIGHTS LEGEND:
 * Use, duplication or disclosure by the Government is subject to restrictions
 * as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
 * and Computer Software clause at DFARS 252.227-7013, and/or in similar or
 * successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
 * rights reserved under the Copyright Laws of the United States.
 */

#define VLSpecificationRelease 1

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <ulocks.h>
#include <sys/types.h>
#include <sys/dmcommon.h>

#define VL_PROTOCOL     1               /* current protocol version */
#define VL_PROTOCOL_REVISION 0          /* current minor version */


/* Basic VL data types for controls, etc. */

typedef int VLBoolean;

typedef struct {
    int x, y;
} VLXY;

typedef struct {
    int base;
    int increment;
    int limit;
    int type;		/* one of the following values: */
#define VL_LINEAR	1
#define VL_GEOMETRIC	2
} VLRange;

typedef struct {
    VLRange numerator;
    VLRange denominator;
} VLFractionRange;

typedef struct {
    int numerator;
    int denominator;
} VLFraction;

/*
 * Definitions for VL nodes:
 */

/*
 * Definitions of node types and kinds. Used as arguments for vlGetNode(),
 * returned in VLNodeInfo.
 */

/* Node type */
#define VL_SRC		1		/* Sources of data */
#define VL_DRN		2		/* Drains for data */
#define VL_DEVICE	3		/* The device node */
#define VL_INTERNAL	4		/* Filters & such */

/* Node kind */
#define VL_VIDEO        1
#define VL_GFX          2		/* Sirius only */
#define VL_MEM          3
#define VL_SCREEN       4
#define VL_TEXTURE      5		/* Sirius only */
#define VL_NETWORK      6
#define VL_BLENDER	7
#define VL_KEYGEN       8               /* Key Generator */
/* also VL_ANY */


typedef int VLNode;

#define VL_NAME_SIZE 32

typedef struct __vlNodeInfo {
    int id;
    char name[VL_NAME_SIZE];	/* name of node */
    int type;			/* see list above */
    int number;			/* number of this node */
    int kind;			/* see list above */
} VLNodeInfo;

typedef int VLPath;		/* handle for a path  */

typedef int VLDev;		/* handle for a device  */

typedef struct __vlDevice {
    VLDev dev;
    char name[VL_NAME_SIZE];	/* name of device */
    int numNodes;		/* number of nodes on this device */
    VLNodeInfo *nodes;		/* list of nodes */
    void *priv;
} VLDevice;

typedef struct __vlDevList {
    int numDevices;		/* number of devices */
    VLDevice *devices;		/* list of devices */
} VLDevList;



/*
 * VL server access routines:
 */

typedef struct _VLServer *VLServer;

extern VLServer vlOpenVideo(
    const char *	/* Server Name */
);

extern int vlCloseVideo(
    VLServer		/* Server Handle */
);

extern int vlGetDeviceList(
    VLServer		/* Server Handle */,
    VLDevList *		/* Device List   */
);

/* Define vlGetFD as an alias for vlConnectionNumber */ 
#define vlGetFD vlConnectionNumber

extern int vlConnectionNumber(VLServer svr);
extern int vlServerProtocolVersion(VLServer svr);
extern int vlServerProtocolRevision(VLServer svr);
extern int vlServerVendorRelease(VLServer svr);
extern char * vlServerString(VLServer svr);



/*
 * Routines to manipulate paths and nodes:
 */

extern VLDev vlGetDevice(
    VLServer            /* Server Handle */,
    VLPath		/* Path Handle */
);

extern VLPath vlCreatePath(
    VLServer		/* Server Handle */,
    VLDev		/* Device Handle */,
    VLNode		/* Source */,
    VLNode		/* Drain */
);

extern VLNode vlGetNode(
    VLServer		/* Server Handle */,
    int			/* node type */,
    int			/* node kind */,
    int			/* node number */
);

extern int vlDestroyPath(
    VLServer		/* Server Handle */,
    VLPath		/* Path Handle */
);

extern int vlAddNode(
    VLServer		/* Server Handle */,
    VLPath		/* Path Handle */,
    VLNode		/* Node */
);

extern int vlRemoveNode(
    VLServer		/* Server Handle */,
    VLPath		/* Path Handle */,
    VLNode		/* Node */
);

/*
 *----------------------------------------------------------------------
 *
 * Controls
 */

typedef int VLControlType;	/* see the following #defines */

/*
 * Configuration controls:
 *
 * VL_DEFAULT_SOURCE(int)	the default input video source for a path
 * VL_TIMING(int)		the timing for video (on video node or device)
 */
#define VL_DEFAULT_SOURCE	0
#define VL_TIMING		1 /* one of the following values */
    /*
     * 525 lines total -> active plus blanking;
     * 625 lines total -> active plus blanking;
     *
     * Betacam/MII/composite tape formats use square pixel analog timings;
     * D1 tape formats use CCIR601 timings;
     * D2 tape formats use the 4 times subcarrier timings.
     */
#define  VL_TIMING_525_SQ_PIX	0 /*analog (525 lines) 12.27 MHz; 646x486 */
#define  VL_TIMING_625_SQ_PIX	1 /*analog (625 lines) 14.75 MHz; 768x576 */
#define  VL_TIMING_525_CCIR601	2 /*digital component  13.50 MHz; 720x486 */
#define  VL_TIMING_625_CCIR601	3 /*digital component  13.50 MHz; 720x576 */
#define  VL_TIMING_525_4FSC	4 /*4x ntsc subcarrier 14.32 MHz; 768x486 */
#define  VL_TIMING_625_4FSC	5 /*4x pal subcarrier  17.72 MHz; 948x576 */

/*
 * Size/Position Controls:
 *
 * These controls are used to set up the size and position of the video
 * stream. The API specification is as follows:
 *
 * On VL_VIDEO nodes:
 *	VL_SIZE(xy)		size of video
 *	VL_OFFSET(xy)		offset to active region of video
 *
 * On all other nodes:
 *	VL_ZOOM(fract)		the zoom factor for the video stream
 *	VL_OFFSET(xy)		the offset within the video (after zoom)
 *	VL_SIZE(xy)		the clipped size of the video
 *	VL_ORIGIN(xy)		the offset within the node
 *	VL_RATE(fract)		frame rate
 *
 * VL_OFFSET, VL_SIZE are measured in pixels against the
 *      extents of the base unit of VL_CAP_TYPE, ie. frames or fields.
 * 
 * VL_RATE is measured in either frames/second or fields/second depending
 *      on the  unit of VL_CAP_TYPE.
 */
#define VL_ORIGIN		2 
#define VL_OFFSET		3		/* pixels per frames or field */
#define VL_SIZE			4		/* pixels per frames or field */
#define VL_RATE			5		/* frames or fields per second */
#define VL_ZOOM			44		/* nominal, 1.0 is passthough */

/*
 * Data type controls:
 *
 * On any node other than video:
 *	VL_CAP_TYPE(int)	what fields/interleaving for the transfer
 *	VL_PACKING(int)		what pixel packing to use
 */
#define VL_CAP_TYPE		6
#define     VL_CAPTURE_NONINTERLEAVED	0	/* fields */
#define     VL_CAPTURE_INTERLEAVED	1	/* frames */
#define     VL_CAPTURE_EVEN_FIELDS	2	/* fields */
#define     VL_CAPTURE_ODD_FIELDS	3	/* fields */
#define VL_PACKING		7
#define     VL_PACKING_RGB_332_P	0
#define     VL_PACKING_RGBA_8		1	/* really ABGR_8 */
#define     VL_PACKING_RGB_8		2	/* really BGR_8 */
#define     VL_PACKING_RBG_323		3
#define     VL_PACKING_VUY_411_SV	4
#define     VL_PACKING_YVYU_422_8	5
#define     VL_PACKING_Y_8_P		6
#define     VL_PACKING_RGB_332		7
#define     VL_PACKING_BGR_332		8
#define     VL_PACKING_RGB_332_IP	9
#define     VL_PACKING_BGR_332_P	10
#define     VL_PACKING_BGR_332_IP	11
#define     VL_PACKING_RGB_565		12
#define     VL_PACKING_RGB_565_P	13
#define     VL_PACKING_RGB_565_IP	14
#define     VL_PACKING_RGB_8_P		15
#define     VL_PACKING_RGB_10		16
#define     VL_PACKING_Y_8_IP		17
#define     VL_PACKING_YUV_444_8	18
#define     VL_PACKING_YUVA_4444_8	19
#define     VL_PACKING_YUV_444_10	20
#define     VL_PACKING_YUVA_4444_10	21	
#define     VL_PACKING_ABGR_8		22	/* really RGBA_8 (OpenGL) */
#define     VL_PACKING_AUYV_8		23
#define     VL_PACKING_A_2_BGR_10	24
#define     VL_PACKING_A_2_UYV_10	25
#define     VL_PACKING_AYU_AYV_10	26	/* two transfers */ 
#define     VL_PACKING_MAX		27

/*
 * Screen-specific controls:
 *	VL_WINDOW(int)		what window the video should appear in
 */
#define VL_WINDOW		8

/*
 * Video attribute controls:
 */
#define VL_BRIGHTNESS		9
#define VL_CONTRAST		10
#define VL_H_PHASE		11
#define VL_HUE			12
#define VL_SATURATION		56
#define VL_RED_SETUP		13		/* boolean */
#define VL_GREEN_SETUP		14		/* boolean */
#define VL_GRN_SETUP		14
#define VL_BLUE_SETUP		15		/* boolean */
#define VL_BLU_SETUP		15
#define VL_ALPHA_SETUP		57		/* boolean */
#define VL_V_PHASE		16		/* in lines, nominal 0 */
#define VL_SIGNAL		17
#define     VL_SIGNAL_NOTHING		0
#define     VL_SIGNAL_BLACK		1
#define     VL_SIGNAL_REAL_IMAGE	2
#define VL_FREEZE		18
#define VL_DEFAULT_DRAIN	19
#define VL_MUXSWITCH		20
#define VL_FORMAT		21
#define     VL_FORMAT_COMPOSITE				0
#define     VL_FORMAT_SVIDEO		 		1
#define     VL_FORMAT_RGB		 		2
#define     VL_FORMAT_BETACAM		 		3
#define     VL_FORMAT_MII		 		4
#define     VL_FORMAT_SMPTE_YUV		 		5
#define     VL_FORMAT_DIGITAL_COMPOSITE	 		6
#define     VL_FORMAT_DIGITAL_COMPONENT	 		7 /* parallel */
#define     VL_FORMAT_DIGITAL_COMPONENT_SERIAL	 	8
#define     VL_FORMAT_DIGITAL_COMPONENT_DUAL	 	9 /* 4:4:4:4  */
#define     VL_FORMAT_DIGITAL_COMPONENT_DUAL_SERIAL	10 /* 4:4:4:4 serial */
#define     VL_FORMAT_DIGITAL_INDYCAM			11
#define     VL_FORMAT_DIGITAL_COMPONENT_RGB		12 /* 4:4:4:4 rgb */
#define     VL_FORMAT_DIGITAL_COMPONENT_RGB_SERIAL	13 /* 4:4:4:4 rgb */
#define VL_SYNC			22
#define     VL_SYNC_INTERNAL		0
#define     VL_SYNC_GENLOCK		2
#define VL_SYNC_SOURCE		58		/* input for VL_SYNC_GENLOCK */

/* Blend functions: not all products support all fcns */
#define VL_BLEND_A_FCN		34
#define VL_BLEND_B_FCN		59
#define     VL_BLDFCN_ZERO              0  /*  0 */
#define     VL_BLDFCN_ONE               1  /*  1 */
#define     VL_BLDFCN_A_ALPHA		2  /*  (A alpha)*/
#define     VL_BLDFCN_B_ALPHA		3  /*  (B alpha)  */
#define     VL_BLDFCN_MINUS_A_ALPHA	4  /*  1 - (A alpha) */
#define     VL_BLDFCN_MINUS_B_ALPHA	5  /*  1 - (B alpha) */
#define VL_BLEND_A		35 /* A source node */
#define VL_BLEND_B		36 /* B source node */
#define VL_BLEND_A_ALPHA	37 /* A alpha node */
#define VL_BLEND_B_ALPHA	38 /* B alpha node */
#define VL_BLEND_A_NORMALIZE	39 /* A follows porter-duff model, boolean */
#define VL_BLEND_B_NORMALIZE	60 /* B follows porter-duff model, boolean */
#define VL_BLEND_OUT_NORMALIZE	61 /* OUT follows porter-duff model, boolean */


/* Additional private controls are in vlpriv.h */


/*
 * Control classes:
 */

typedef unsigned int VLControlClass; /* one of the following values: */

#define VL_CLASS_NO_UI		0
#define VL_CLASS_SLIDER         1
#define VL_CLASS_KNOB           2
#define VL_CLASS_BUTTON         3
#define VL_CLASS_TOGGLE         4
#define VL_CLASS_DETENT_KNOB    5
#define VL_CLASS_LIST		6

#define VL_CLASS_RDONLY		0x8000	/* control is read-only */
#define VL_CLASS_WRONLY		0x4000	/* control is write-only */

#define VL_IS_CTL_RDONLY(x)	((x)->class & VL_CLASS_RDONLY)
#define VL_IS_CTL_WRONLY(x)	((x)->class & VL_CLASS_WRONLY)
#define VL_IS_CTL_RW(x)		(!(VL_IS_CTL_RDONLY(x) && VL_IS_CTL_WRONLY(x)))

#define VL_CLASS_MASK		0xfff


typedef unsigned int VLControlGroup;  /* one of the following values: */

#define VL_CTL_GROUP_BLENDING			0
#define VL_CTL_GROUP_VISUALQUALITY		1
#define VL_CTL_GROUP_SIGNAL			2
#define VL_CTL_GROUP_CODING			3
#define VL_CTL_GROUP_SYNC			4
#define VL_CTL_GROUP_ORIENTATION		5
#define VL_CTL_GROUP_SIZING			6
#define VL_CTL_GROUP_RATES			7
#define VL_CTL_GROUP_WS				 8	/* Window System */
#define VL_CTL_GROUP_PATH			 9	/* Path Controls */
#define VL_CTL_GROUP_SIGNAL_ALL			10	/* comp & component */
#define VL_CTL_GROUP_SIGNAL_COMPOSITE		11	/* Controls composite */
#define VL_CTL_GROUP_SIGNAL_COMPONENT		12	/* Controls component */
#define VL_CTL_GROUP_SIGNAL_CLUT_COMPOSITE	13	/* CLUT composite */
#define VL_CTL_GROUP_SIGNAL_CLUT_COMPONENT	14	/* CLUT component */
#define VL_CTL_GROUP_KEYING			15

#define VL_CTL_GROUP_PRO		0x8000	/* control is a Pro Control */

#define VL_IS_CTL_PRO(x)	((x)->group & VL_CTL_GROUP_PRO)
#define VL_IS_CTL_COMMON(x)	(!((x)->group & VL_CTL_GROUP_PRO))

#define VL_CTL_GROUP_MASK		0xfff


typedef int VLControlValueType;	/* one of the following values: */

#define VL_CT_FRACT	1
#define VL_CT_BOOL	2
#define VL_CT_INT	3
#define VL_CT_UINT	4
#define VL_CT_XY	5


typedef struct {
    char name[VL_NAME_SIZE];
    int value;
} VLControlItem;

typedef struct __vlControlInfo {
    char name[VL_NAME_SIZE]; /* name of control */
    VLControlType type;      /* e.g. WINDOW, HUE, BRIGHTNESS */
    VLControlClass ctlClass; /* SLIDER, DETENT, KNOB, BUTTON */
    VLControlGroup group;    /* BLEND, VISUAL QUALITY, SIGNAL, SYNC */
    VLNode node;             /* associated node */
    VLControlValueType valueType;	/* what kind of data do we have */
    int valueCount;		/* how many data items do we have */
    int numFractRanges;     /* number of ranges to describe control */
    VLFractionRange *ranges; /* range of values of control */

    int numItems;		/* number of enumerated items */
    VLControlItem *itemList;	/* the actual enumerations */
} VLControlInfo;

/* Structure used by the VL to store the size of the different controls: */
typedef union {
    VLFraction	fractVal;
    VLBoolean	boolVal;
    int		intVal;
    VLXY	xyVal;
    char	stringVal[96];  /* beware of trailing NULLs! */
    float	matrixVal[3][3];	
    uint	pad[24];	/* reserved */
} VLControlValue;

typedef struct {
    int numControls;
    VLControlInfo *controls;
} VLControlList;

extern int           vlGetControl(
    VLServer		/* Server Handle */,
    VLPath		/* Path Handle */,
    VLNode		/* node */,
    VLControlType	/* Type, from list above */,
    VLControlValue *	/* the value to be returned */
);

extern VLControlInfo *vlGetControlInfo(
    VLServer		/* Server Handle */,
    VLPath		/* Path Handle */,
    VLNode		/* Source/Drain */,
    VLControlType	/* Type, from list above */
);

extern int vlFreeControlInfo(
    VLServer		/* Server Handle */,
    VLControlInfo *	/* Pointer from vlGetControlInfo */
);

extern VLControlList *vlGetControlList(
    VLServer		/* Server Handle */,
    VLPath		/* Path Handle */
);

extern int vlFreeControlList(
    VLServer		/* Server Handle */,
    VLControlList *	/* Pointer from vlGetControlList */
);

extern int vlSetControl(
    VLServer		/* Server Handle */,
    VLPath		/* Path Handle */,
    VLNode		/* node */,
    VLControlType	/* Type, from list above */,
    VLControlValue *	/* the value to be set */
);

/*----------------------------------------------------------------------*/

/*
 * Path usage:
 */

typedef VLPath *VLPathList;

typedef short VLUsageType;

#define VL_DONE_USING	0x0000
#define VL_READ_ONLY	0x0001
#define VL_SHARE	0x0002
#define VL_LOCK	0x0003
#define VL_MAX_USAGE	0x0004

extern int vlSetupPaths(
    VLServer		/* Server Handle */,
    VLPathList          /* list of Path Handles */,
    int                 /* Number of PathHandles */,
    VLUsageType         /* CtrlUsage */,
    VLUsageType         /* StreamUsage */
);

typedef uint VLEventMask;

/*
 * Transfer modes:
 */

#define VL_TRANSFER_MODE_CONTINUOUS	101
#define VL_TRANSFER_MODE_DISCRETE	102
#define VL_TRANSFER_MODE_AUTOTRIGGER	103

#define VLTriggerImmediate	VLNoEventsMask

typedef struct VLTransferDescriptor {
    int mode;
    VLEventMask trigger;
    short delay;
    short count;
    int sequence;
    unsigned long long ustime;
} VLTransferDescriptor;

extern int vlBeginTransfer(
    VLServer		/* Server Handle */,
    VLPath		/* Path Handle */,
    int		/* Count of descriptors */,
    VLTransferDescriptor *	/* array of transfer descriptors */
);

extern int vlEndTransfer(
    VLServer		/* Server Handle */,
    VLPath		/* Path Handle */
);

/*
 * Events
 */

typedef void (*VLEventHandler)(uint, void *);
typedef int  (*VLPendingFunc)(void *);

#define VLStreamBusyMask	   (1<< 0)    /* Stream is locked */
#define VLStreamPreemptedMask	   (1<< 1)    /* Stream was grabbed */
#define VLAdvanceMissedMask	   (1<< 2)    /* Already reached time */
#define VLStreamAvailableMask	   (1<< 3)    /* Stream has been released */
#define VLSyncLostMask		   (1<< 4)    /* Sync isn't being detected */
#define VLStreamStartedMask	   (1<< 5)    /* Stream started delivery */
#define VLStreamStoppedMask	   (1<< 6)    /* Stream stopped delivery */
#define VLSequenceLostMask	   (1<< 7)    /* A Field/Frame dropped */
#define VLControlChangedMask	   (1<< 8)    /* A Control has changed */
#define VLTransferCompleteMask	   (1<< 9)    /* A Transfer has completed */
#define VLTransferFailedMask	   (1<<10)    /* A Transfer has failed */
#define VLEvenVerticalRetraceMask  (1<<11)    /* A Vertical Retrace event */
#define VLOddVerticalRetraceMask   (1<<12)    /* A Vertical Retrace event */
#define VLFrameVerticalRetraceMask (1<<13)    /* A Vertical Retrace event */
#define VLDeviceEventMask          (1<<14)    /* A Vertical Retrace event */
#define VLDefaultSourceMask        (1<<15)    /* Default Source Changed */
#define VLControlRangeChangedMask  (1<<16)
#define VLControlPreemptedMask     (1<<17)
#define VLControlAvailableMask     (1<<18)
#define VLDefaultDrainMask         (1<<19)    /* Default Drain Changed */

#define VLStreamBusy		2	    /* Start with 2 since 0 and 1 */
#define VLStreamPreempted	3	    /* are reserved for daemon */
#define VLAdvanceMissed		4
#define VLStreamAvailable	5
#define VLSyncLost		6
#define VLStreamStarted		7
#define VLStreamStopped		8
#define VLSequenceLost		9
#define VLControlChanged	10
#define VLTransferComplete	11
#define VLTransferFailed	12
#define VLEvenVerticalRetrace	13
#define VLOddVerticalRetrace	14
#define VLFrameVerticalRetrace	15
#define VLDeviceEvent		16
#define VLDefaultSource		17
#define VLControlRangeChanged	18
#define VLControlPreempted	19
#define VLControlAvailable	20
#define VLDefaultDrain		21

#define VLLASTEvent		(128) /* must be bigger than any event # */

#define VLAllEventsMask		(~0)
#define VLNoEventsMask		0

typedef struct __vlAnyEvent {
    int reason;
    VLServer server;
    VLDev device;
    VLPath path;
    uint time;
    uint serial;
} VLAnyEvent;

typedef VLAnyEvent VLStreamBusyEvent;
typedef VLAnyEvent VLStreamPreemptedEvent;
typedef VLAnyEvent VLAdvanceMissedEvent;
typedef VLAnyEvent VLStreamAvailableEvent;
typedef VLAnyEvent VLSyncLostEvent;
typedef VLAnyEvent VLStreamStartedEvent;
typedef VLAnyEvent VLStreamStoppedEvent;
typedef VLAnyEvent VLOddVerticalRetraceEvent;
typedef VLAnyEvent VLEvenVerticalRetraceEvent;
typedef VLAnyEvent VLFrameVerticalRetraceEvent;
typedef VLAnyEvent VLTransferCompleteEvent;
typedef VLAnyEvent VLTransferFailedEvent;

typedef struct __vlSequenceLostEvent {
    int reason;
    VLServer server;
    VLDev device;
    VLPath path;
    uint time;
    int serial;
    VLBoolean field;	/* TRUE == field, FALSE == frame */
    int number;		/* sequence number of dropped field or frame */
    int count;		/* number of dropped fields or frames */
} VLSequenceLostEvent;

typedef struct __vlControlChangedEvent {
    int reason;
    VLServer server;
    VLDev device;
    VLPath path;
    uint time;
    int serial;
    VLNode node;
    VLControlType type;
    VLControlValue value;
} VLControlChangedEvent;

typedef struct __vlControlRangeChangedEvent {
    int reason;
    VLServer server;
    VLDev device;
    VLPath path;
    uint time;
    int serial;
    VLNode node;
    VLControlType type;
} VLControlRangeChangedEvent;

typedef VLControlRangeChangedEvent VLControlPreemptedEvent;
typedef VLControlRangeChangedEvent VLControlAvailableEvent;

typedef struct __vlDeviceEventEvent {
    int reason;
    VLServer server;
    VLDev device;
    VLPath path;
    uint time;
    int serial;
    int value;		/* device-specific info */
} VLDeviceEventEvent;

typedef struct __vlDefaultNodeEvent {
    int reason;
    VLServer server;
    VLDev device;
    VLPath path;
    uint time;
    int serial;
    VLNode node;
} VLDefaultNodeEvent;

typedef VLDefaultNodeEvent VLDefaultSourceEvent;
typedef VLDefaultNodeEvent VLDefaultDrainEvent;

typedef struct __vlErrorEvent {
    int reason;
    VLServer server;
    VLDev device;
    VLPath path;
    uint time;
    int serial;

    int errorCode;
    int resourceId;
    int majorCode;
    int minorCode;
} VLErrorEvent;

typedef union __vlEvent {
    int reason;
    VLAnyEvent vlany;
    VLStreamBusyEvent vlstreambusy;
    VLStreamPreemptedEvent vlstreampreempted;
    VLAdvanceMissedEvent vladvancemissed;
    VLStreamAvailableEvent vlstreamavailable;
    VLSyncLostEvent vlsynclost;
    VLStreamStartedEvent vlstreamstarted;
    VLStreamStoppedEvent vlstreamstopped;
    VLSequenceLostEvent vlsequencelost;
    VLControlChangedEvent vlcontrolchanged;
    VLControlRangeChangedEvent vlcontrolrangechanged;
    VLControlPreemptedEvent vlcontrolpreempted;
    VLControlAvailableEvent vlcontrolavailable;
    VLTransferCompleteEvent vltransfercomplete;
    VLTransferFailedEvent vltransferfailed;
    VLEvenVerticalRetraceEvent vlevenverticalretrace;
    VLOddVerticalRetraceEvent vloddverticalretrace;
    VLFrameVerticalRetraceEvent vlframeverticalretrace;
    VLDeviceEventEvent vldeviceevent;
    VLDefaultSourceEvent vldefaultsource;
    VLDefaultDrainEvent vldefaultdrain;
    VLErrorEvent vlerror;
} VLEvent;

extern int vlPending(
    VLServer		/* Server */
);

extern int vlSelectEvents(
    VLServer		/* Server Handle */,
    VLPath		/* Path Handle */,
    VLEventMask	/* Events of Interest */
);

extern int vlRegisterHandler(
    VLServer		/* Server */,
    int			/* fd */,
    VLEventHandler	/* Event Handler */,
    VLPendingFunc	/* pending check if there is one */,
    void *		/* user data */
);

extern int vlRemoveHandler(
    VLServer		/* Server */,
    int			/* fd */
);

extern void vlMainLoop(
    void
);

extern char *vlEventToName(
    int		/* Event Type */
);

extern int vlNextEvent(
    VLServer		/* Server */,
    VLEvent *		/* Event Return Value */
);

extern int vlPeekEvent(
    VLServer		/* Server */,
    VLEvent *		/* Event Return Value */
);

extern int vlCheckEvent(
    VLServer		/* Server */,
    VLEventMask	/* mask */,
    VLEvent *		/* Event Return Value */
);

typedef void (*VLCallbackProc)(
    VLServer		/* Server Handle */,
    VLEvent *		/* Event Structure Pointer */,
    void *		/* Client Data */
);

extern int vlAddCallback(
    VLServer		/* Server Handle */,
    VLPath		/* Path Handle */,
    VLEventMask	/* Events on which to invoke callbacks */,
    VLCallbackProc	/* Callback Procedure */,
    void *		/* Client Data */
);

extern int vlRemoveCallback(
    VLServer		/* Server Handle */,
    VLPath		/* Path Handle */,
    VLEventMask	/* Events from which to remove callbacks */,
    VLCallbackProc	/* Callback Procedure */,
    void *		/* Client Data */
);

extern int vlRemoveAllCallbacks(
    VLServer		/* Server Handle */,
    VLPath	/* Path Handle */,
    VLEventMask	/* Events on which to invoke callbacks */
);

extern int vlCallCallbacks(
    VLServer		/* Server Handle */,
    VLEvent *		/* event for call backs */
);

/*
 * Convenience routines
 */

typedef struct _DMRingBuffer *VLBuffer;
typedef char * VLInfoPtr;

extern int vlGetTransferSize(
    VLServer		/* Server Handle */,
    VLPath		/* Path Handle */
);

int vlRegisterBuffer(
    VLServer		/* Server Handle */,
    VLPath		/* Path Handle */,
    VLNode		/* Memory node id */,
    VLBuffer		/* ring buffer handle from dmRBCreate */
);

int vlDeregisterBuffer(
    VLServer		/* Server Handle */,
    VLPath		/* Path Handle */,
    VLNode		/* Memory node id */,
    VLBuffer		/* the ring buffer handle */
);

VLBuffer vlCreateBuffer(
    VLServer		/* The Server Handle */,
    VLPath		/* The Path Handle */,
    VLNode		/* the Node Handle */,
    int			/* number of frames in the buffer */
);

int vlBufferAdvise(
    VLBuffer		/* the ring buffer handle */,
    int			/* Advisory type */
);

#define VL_CA_TAG_GAMMA  1	/* parm is a double * */
#define VL_CA_TAG_MATRIX 2      /* parm is a 3x4 matrix of coefficients */
#define VL_CA_TAG_FLAG	 3	/* parm is a random flag */

int vlConversionAdviseGamma(
    VLServer svr,
    VLPath   path,	       
    int      sourcePacking,
    int      destPacking,
    double   gamma
);		       

int vlConversionAdviseMatrix(
    VLServer svr,
    VLPath   path,	       
    int      sourcePacking,
    int      destPacking,
    double   *matrix
);		       

int vlConversionAdviseFlag(
    VLServer svr,
    VLPath   path,	       
    int      sourcePacking,
    int      destPacking,
    int      flag
);		       

#define VL_BUFFER_ADVISE_NOACCESS 1 /* Contents of buffer are not accessed */
#define VL_BUFFER_ADVISE_ACCESS   2 /* Contents of buffer will be accessed */

int vlBufferGetFd(
    VLBuffer		/* Buffer handle */
);

VLInfoPtr vlGetNextValid(
    VLServer		/* Server Handle */,
    VLBuffer		/* Buffer Handle */
);

VLInfoPtr vlGetLatestValid(
    VLServer		/* Server Handle */,
    VLBuffer		/* Buffer Handle */
);

VLInfoPtr vlGetNextFree(
    VLServer		/* Server Handle */,
    VLBuffer		/* Buffer Handle */,
    int			/* size of the buffer */
);

void * vlGetActiveRegion(
    VLServer		/* Server Handle */,
    VLBuffer		/* Buffer Handle */,
    VLInfoPtr info	/* info handle */
);

DMediaInfo * vlGetDMediaInfo(
    VLServer		/* Server Handle */,
    VLBuffer		/* Buffer Handle */,
    VLInfoPtr		/* Info Handle */
);

DMImageInfo * vlGetImageInfo(
    VLServer		/* Server Handle */,
    VLBuffer		/* Buffer Handle */,
    VLInfoPtr		/* Info Handle */
);

int vlPutFree(
    VLServer		/* Server Handle */,
    VLBuffer		/* Buffer handle */
);

int vlPutValid(
    VLServer		/* Server Handle */,
    VLBuffer		/* Buffer handle */
);

int vlBufferDone(
    VLBuffer		/* Buffer Handle */
);

int vlBufferReset(
    VLServer		/* Server Handle */,
    VLBuffer		/* Buffer Handle */
);

int vlDestroyBuffer(
    VLServer		/* Server Handle */,
    VLBuffer		/* Buffer Handle */
);

extern uint vlInitDevice(
    VLServer		/* Server Handle */,
    VLDev		/* Device Handle */,
    const char *	/* path name to defaults file, or NULL */
);


/*
 * Default management routines
 */

extern int vlSaveSystemDefaults(
    VLServer		/* Server */
);

extern int vlRestoreSystemDefaults(
    VLServer		/* Server */
);

extern int vlRestoreFactoryDefaults(
    VLServer		/* Server */
);


/*
 * Fraction Utility Routines
 */

VLBoolean vlMatchFloat (
    double given              /* in: the floating point value to compare */,
    VLFractionRange *ranges   /* in: the range list to compare with */,
    int nranges               /* in: the number of ranges in the list */,
    VLFraction *lower         /* out: the closest lower fraction */,
    VLFraction *higher        /* out: the closest higher fraction */
);

VLBoolean vlMatchFraction (
    VLFraction *given         /* in: the fraction to compare */,
    VLFractionRange *ranges   /* in: the range list to compare with */,
    int nranges               /* in: the number of ranges in the list */,
    VLFraction *lower         /* out: the closest lower fraction */,
    VLFraction *higher        /* out: the closest higher fraction */
);

int vlFractionCount (
    VLFractionRange *ranges,
    int nranges
);

VLBoolean vlMatchFractionInList (
    VLFraction *fraction      /* in: the fraction to find */,
    VLFraction *list          /* in: the list to search */,
    int count                 /* in: length of list */,
    VLFraction *lower         /* out: nearest lower fraction */,
    VLFraction *higher        /* out: nearest higher fraction */,
    int *index_return         /* out: index of lower */
);

VLFraction * vlFractionList(
    VLFractionRange *range ,
    int nranges ,
    int *length_return
);

void vlChooseClosestFraction(
    VLFraction *lower,
    VLFraction *higher,
    VLFraction *user
);



/*
 * The following are the error code definitions for the VL.
 * The error code will be stored in vlErrno on an error return.
 */

/*
 * Do not use vlErrno, as it is here only for backwards compatibility.
 * instead use the call vlGetErrno
 */
extern int vlErrno;

#define VLSuccess            0    /* everything's okay */
#define VLBadRequest         1    /* bad request code */
#define VLBadValue           2    /* int parameter out of range */
#define VLBadPath            3    /* parameter not a path */
#define VLBadNode            4    /* parameter not a node */
#define VLBadAtom            5    /* parameter not an atom */
#define VLBadDevice          6    /* parameter not a device */
#define VLBadControl         7    /* parameter not a control */
#define VLBadMatch           8    /* parameter mismatch */
#define VLBadAccess         10    /* depending on context */
#define VLBadAlloc          11    /* insufficient resources */
#define VLBadIDChoice       14    /* choice not in range or already used */
#define VLBadName           15    /* font or color name doesn't exist */
#define VLBadLength         16    /* Request length incorrect */
#define VLBadIoctl          17	  /* ioctl failed in the server */
#define VLBadImplementation 18    /* server is broken */
#define VLPathInUse	    19	  /* Path is in exclusive use */
#define VLBadServer	    20	  /* Server parameter is invalid */
#define VLBadBuffer	    21    /* Buffer invalid */
#define VLBadSize	    22	  /* Buffer size invalid */
#define VLNotEnoughResident 23	  /* we don't have enough resident memory */
#define VLBufferTooSmall    24	  /* the buffer allocated is too small */
#define VLValueOutOfRange   25	  /* the value is out of range (for controls)*/
#define VLSetupFailed       26    /* SetupPath of failed  */
#define VLBadWinAlloc       27    /* Insufficient hardware screen space */
#define VLInputsNotLocked   28    /* Video input sources cannot be locked */

#define VLMaxError	    29	  /* Must be larger than the largest error */
				  /* Subject to change in future releases */

extern int vlGetErrno(
    void
);

extern const char *vlStrError(
    int 		/* error value */
);

extern void vlPerror(
    const char *	/* string prefixed before error message */
);

typedef int (*VLErrorHandler) (
    VLServer		/* Server Handle */,
    VLEvent*		/* Error event */
);

extern VLErrorHandler vlSetErrorHandler (
    VLErrorHandler	/* Handler */
);

typedef int (*VLIOErrorHandler) (
    VLServer             /* Server Handle */
);

extern VLIOErrorHandler vlSetIOErrorHandler (
    VLIOErrorHandler    /* handler */
);


/*
 * Reserved resource and constant definitions:
 */

#define VL_ANY		0x10000000	/* also used by other VL routines
					 * to represent an unknown value. */
#define VL_IRISGLWINDOW	0x10000010

#define VLNone               0L /* universal null resource or null atom */

#define VLAllTemporary       0L /* special Resource ID passed to KillClient */

#define VLCurrentTime        0L /* special Time */

#ifndef TRUE
#define TRUE	(1)
#endif

#ifndef FALSE
#define FALSE	(0)
#endif

#ifdef __cplusplus
}
#endif
#endif /* _VL_H_ */
