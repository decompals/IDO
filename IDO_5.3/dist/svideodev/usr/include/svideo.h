/*
 * IndigoVideo library interface
 *
 * Copyright 1992, Silicon Graphics, Inc.
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

#ident "$Revision: 2.21 $"

#ifndef __SVIDEO_H__
#define __SVIDEO_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/types.h>
#include <sys/time.h>

typedef int	boolean;

#ifndef TRUE
#define TRUE	1
#endif

#ifndef FALSE
#define FALSE	0
#endif


#define SV_NTSC_XMAX		640
#define SV_NTSC_YMAX		480
#define SV_PAL_XMAX		768
#define SV_PAL_YMAX		576

/*
 * Because starter video board packs YUV pixels from line pairs into one word,
 * blanking buffer must be defined as an even number of lines.
 */
#define SV_NTSC_BLANKING_BUFFER_SIZE	 (45+1)
#define SV_PAL_BLANKING_BUFFER_SIZE	 (49+1)

#define SV_MAX_SOURCES	2

/* Mode parameter for svBind{GL}Window calls */
#define SV_IN_OFF	0	        /* No Video */
#define SV_IN_OVER	1	        /* Video over graphics */
#define SV_IN_UNDER	2	        /* Video under graphics */
#define SV_IN_REPLACE	3	        /* Video replaces entire win */

/* Mode parameters for svLoadMap(). Specifies buffer, always 256 entries. */
#define SV_INPUT_COLORMAP	0           /* tuples of 8-bit RGB */
#define SV_CHROMA_KEY_MAP	1           /* tuples of 8-bit RGB */
#define SV_COLOR_SPACE_MAP	2           /* tuples of 8-bit RGB */
#define SV_GAMMA_MAP		3           /* tuples of 24-bit red values */
#define SV_CMAP_SIZE		256

typedef struct _rgb_tuple {
    unsigned short red;
    unsigned short blue;
    unsigned short green;
} rgb_tuple, SVcolorMap[SV_CMAP_SIZE];

/* Mode parameters for svUseExclusive() */
#define SV_INPUT                0
#define SV_OUTPUT               1
#define SV_IN_OUT               2

/* Format constants for the capture routines  */
#define SV_RGB8_FRAMES   0	/* noninterleaved 8-bit 3:2:3 RBG fields */
#define SV_RGB32_FRAMES  1	/* 32-bit 8:8:8 RGB frames */
#define SV_YUV411_FRAMES 2	/* interleaved, 8:2:2 YUV format */
#define SV_YUV411_FRAMES_AND_BLANKING_BUFFER   3

/* Macro to determine bit vector size in bytes for svCaptureBurst() */
#define SV_BITVEC_SIZE(numframes) ((((numframes) + 7)/8) * 2)
/*
 * Macro to assist with handling bit vector returned by svCaptureBurst().
 * Returns value of even/odd bit from bitvector "bv" for field "f".
 */
#define SV_GET_FIELD(bv,f) ((*((u_char *)(bv) + (f)/8) >> ((f)%8)) & 1)
#define SV_EVEN_FIELD	0	/* field is all even-numbered lines of frame */
#define SV_ODD_FIELD	1	/* field is all odd-numbered lines of frame */

/* Error codes */

extern int svideo_errno;

#define SV_STATUS_OK	0
#define SV_STATUS_ERR	-1

#define SV_BAD_HANDLE	1		/* bad pointer */
#define SV_BADOPEN	2		/* unable to open video device */
#define SV_BADSTAT	3		/* bad stat of video device */
#define SV_NODEV	4		/* video device doesn't exist */
#define SV_BAD_INFO	5		/* bad info call on video driver */
#define SV_BAD_ATTACH	6		/* unable to attach to video device */
#define SV_NO_MEM	7		/* no memory available */
#define SV_NO_GL	8		/* no GL support */
#define SV_BAD_VALUE	9		/* Bad value of argument to routine */
#define SV_NO_WINDOW	10		/* svBindWindow not done yet */
#define SV_NO_INIT_CAP	11		/* svInitCapture not done yet */
#define SV_INIT_CAP	12		/* cannot call after svInitCapture */
#define SV_EXCLUSIVE	13		/* board already in exclusive mode */
#define SV_NO_X         14              /* no X server with video available */
#define SV_LAST_ERROR   14

/*
 * svSetParam is passed variable-length argument lists,
 * consisting of <name, value> pairs.   The following
 * constants identify argument names and values.
 */
#define	SV_NAME_BASE		1000
#define	SV_SOURCE		(SV_NAME_BASE + 0)
#define	    SV_SOURCE1		0
#define	    SV_SOURCE2		1
#define	    SV_SOURCE3		2
#define	SV_COLOR		(SV_NAME_BASE + 1)
#define	    SV_DEFAULT_COLOR	0
#define	    SV_USER_COLOR	1
#define	    SV_MONO	        2
#define	SV_OUTPUTMODE		(SV_NAME_BASE + 2)
#define	    SV_LIVE_OUTPUT	0
#define	    SV_STILL24_OUT	1
#define	SV_FREEZE		(SV_NAME_BASE + 3)
#define	SV_DITHER		(SV_NAME_BASE + 4)
#define	SV_OUTPUT_FILTER	(SV_NAME_BASE + 5)
#define	SV_HUE			(SV_NAME_BASE + 6)
#define	SV_GENLOCK		(SV_NAME_BASE + 7)
#define	    SV_GENLOCK_OFF	0
#define	    SV_GENLOCK_ON	1
#define	    SV_GENLOCK_HOUSE	2
#define	SV_BROADCAST		(SV_NAME_BASE + 8)
#define	    SV_NTSC		0
#define	    SV_PAL		1
#define	SV_VIDEO_MODE		(SV_NAME_BASE + 9)
#define	    SV_COMP		0
#define	    SV_SVIDEO		1
#define	SV_INPUT_BYPASS		(SV_NAME_BASE + 10)
#define	SV_FIELDDROP		(SV_NAME_BASE + 11)
#define	SV_SLAVE		(SV_NAME_BASE + 12)
#define	SV_APERTURE_FACTOR	(SV_NAME_BASE + 13)
#define	    SV_AFACTOR_0	0
#define	    SV_AFACTOR_QTR	1
#define	    SV_AFACTOR_HLF	2
#define	    SV_AFACTOR_ONE	3
#define	SV_CORING		(SV_NAME_BASE + 14)
#define	    SV_COR_OFF		0
#define	    SV_COR_1LSB		1
#define	    SV_COR_2LSB		2
#define	    SV_COR_3LSB		3
#define	SV_APERTURE_BANDPASS	(SV_NAME_BASE + 15)
#define	    SV_ABAND_F0		0
#define	    SV_ABAND_F1		1
#define	    SV_ABAND_F2		2
#define	    SV_ABAND_F3		3
#define	SV_PREFILTER		(SV_NAME_BASE + 16)
#define	SV_CHROMA_TRAP		(SV_NAME_BASE + 17)
#define	SV_CK_THRESHOLD		(SV_NAME_BASE + 18)
#define	SV_PAL_SENSITIVITY	(SV_NAME_BASE + 19)
#define	SV_GAIN_CONTROL		(SV_NAME_BASE + 20)
#define	    SV_GAIN_SLOW	0
#define	    SV_GAIN_MEDIUM	1
#define	    SV_GAIN_FAST	2
#define	    SV_GAIN_FROZEN	3
#define	SV_AUTO_CKILL		(SV_NAME_BASE + 21)
#define	SV_VTR_MODE		(SV_NAME_BASE + 22)
#define	    SV_VTR_INPUT	0
#define	    SV_CAMERA_INPUT	1
#define	SV_LUMA_DELAY		(SV_NAME_BASE + 23)
#define	SV_VNOISE		(SV_NAME_BASE + 24)
#define	    SV_VNOISE_NORMAL	0
#define	    SV_VNOISE_SEARCH	1
#define	    SV_VNOISE_AUTO	2
#define	    SV_VNOISE_BYPASS	3
#define	SV_CHCV_PAL		(SV_NAME_BASE + 25)
#define	SV_CHCV_NTSC		(SV_NAME_BASE + 26)
#define	SV_CCIR_LEVELS		(SV_NAME_BASE + 27)
#define	SV_STD_CHROMA		(SV_NAME_BASE + 28)
#define	SV_DENC_VTBYPASS	(SV_NAME_BASE + 29)
#define	SV_FAST_TIMECONSTANT	(SV_NAME_BASE + 30)
#define	SV_GENLOCK_DELAY	(SV_NAME_BASE + 31)
#define	SV_PHASE_SYNC		(SV_NAME_BASE + 32)
#define	SV_VIDEO_OUTPUT		(SV_NAME_BASE + 33)
#define	SV_CHROMA_PHASEOUT	(SV_NAME_BASE + 34)
#define	SV_CHROMA_CENTER	(SV_NAME_BASE + 35)
#define	SV_YUV_TO_RGB_INVERT	(SV_NAME_BASE + 36)
#define	SV_SOURCE1_BROADCAST	(SV_NAME_BASE + 37)
#define	SV_SOURCE1_MODE		(SV_NAME_BASE + 38)
#define	SV_SOURCE2_BROADCAST	(SV_NAME_BASE + 39)
#define	SV_SOURCE2_MODE		(SV_NAME_BASE + 40)
#define	SV_SOURCE3_BROADCAST	(SV_NAME_BASE + 41)
#define	SV_SOURCE3_MODE		(SV_NAME_BASE + 42)
#define	SV_SIGNAL_STD		(SV_NAME_BASE + 43)
#define	    SV_NOSIGNAL		2
#define	SV_SIGNAL_COLOR		(SV_NAME_BASE + 44)

typedef struct svCaptureInfo {
    int       format;           /* YUV or RGB.  Default is RGB. */
    int       width;            /* For decimation. 0 means use current,
				 * default is 0. */
    int       height;           /* For decimation.  0 means use current,
				 * default is 0. */
    int       size;             /* Queue size or number of frames to capture.
				 * default is 1. */
    int       samplingrate;     /* Only used by continuous capture. */
} svCaptureInfo;


/* Public routine prototypes: */

struct svideo_node;
typedef struct svideo_node *SVhandle;

extern SVhandle    svOpenVideo(void);
extern int         svCloseVideo(SVhandle);
extern int	   svIsVideoDisplayed(SVhandle);
extern int	   svSetSize(SVhandle V, int, int);
extern int	   svQuerySize(SVhandle V, int, int, int *, int *);
extern int	   svWindowOffset(SVhandle V, int, int);
extern int	   svPutFrame(SVhandle, char *);
extern int	   svLoadMap(SVhandle, int, rgb_tuple *);
extern int	   svSetParam(SVhandle, long *, int);
extern int	   svSetStdDefaults(SVhandle);
extern int	   svGetParam(SVhandle, long *, int);
extern int	   svGetParamRange(SVhandle, long *, int);
extern int	   svOutputOffset(SVhandle, int, int);
extern int         svUseExclusive(SVhandle, boolean, int);
extern int         svFindVisibleRegion(SVhandle, void *, void **, int);
extern void        svPerror(char *);
extern char      * svStrerror(int);
extern int         svCaptureBurst(SVhandle, svCaptureInfo *, void *, void *);
extern int         svCaptureOneFrame(SVhandle, int, int *, int *, void *);
extern int         svInitContinuousCapture(SVhandle, svCaptureInfo *);
extern int         svGetCaptureData(SVhandle, void **, long *);
extern int         svUnlockCaptureData(SVhandle, void *);
extern int         svEndContinuousCapture(SVhandle);
extern int         svQueryCaptureBufferSize(SVhandle, svCaptureInfo *, int *);

extern int         svBindGLWindow(SVhandle, long, int);

extern void        svInterleaveFields(boolean, char *, char *, int, int);
extern void        svYUVtoRGB(boolean, char *, long *, int, int);
extern void        svRGB8toRGB32(boolean, char *, long *, int, int);


/*
 * X11/GL event handling:
 *
 *   X11 applications:
 *	XSelectInput(display, win, StructureNotifyMask);
 *	while (1) {
 *	    XNextEvent(display, &event);
 *	    if (event.type == SvVideoActivityEventNumber) {
 *	       SVvideoActivityEvent *ev = (SVvideoActivityEvent *)&event;
 *	       if (ev->reason == SvVideoStarted)
 *	       else if (ev->reason  == ...
 *	    } else if (event.type == SvParamChangeEventNumber) {
 *	       SVparamChangeEvent *ev = (SVparamChangeEvent *)&event;
 *	       if (ev->attribute == SvActiveAttribute)
 *	       else if (ev->attribute ==  ...
 *	    }
 *	}
 *
 * GL applications: 
 *	qdevice(VIDEO);
 *	if (qread(&val) == VIDEO) {
 *	    if (val == SvVideoStarted)
 *	    else if (val == SvEncodingAttribute)
 *	    else if (val == ...
 *	}
 *
 */

/* Values for XEvent type */
extern int SvVideoActivityEventNumber;	/* detail in SVvideoActivityEvent */
extern int SvParamChangeEventNumber;	/* detail in SVparamChangeEvent */

/* SVvideoActivityEvent reasons */
extern int SvVideoStarted;
extern int SvVideoStopped;
extern int SvVideoBusy;
extern int SvVideoPreempted;

/* SVparamChangeEvent attributes */
extern int SvActiveAttribute;		/* value always 0 */
extern int SvEncodingAttribute;		/* values declared below */
extern int SvFreezeAttribute;		/* 0 = off, 1 = on */
extern int SvSourceAttribute;		/* 0,1,2 => input ports 1,2,3 */
extern int SvParamChangeAttribute;	/* value always 1 */ 

/* SvEncodingAttribute values */
extern int SvNTSCComposite;
extern int SvPALComposite;
extern int SvNTSCSVideo;
extern int SvPALSVideo;


#ifdef _XLIB_H_

/* Declare the following if <X11/Xlib.h> has been included */

extern int	    svBindWindow(SVhandle, Display *, Window, int);
extern int	    svSelectXEvents(SVhandle, Display *);

typedef struct {
  int type;
  unsigned long serial;    /* # of last request processed by server */
  Bool send_event;         /* true if this came from a SendEvent request */
  Display *display;        /* Display the event was read from */
  Drawable drawable;       /* drawable */
  unsigned long reason;    /* what generated this event */
  long unused1;
  Time time;               /* milliseconds */
} SVvideoActivityEvent;

typedef struct {
  int type;
  unsigned long serial;	   /* # of last request processed by server */
  Bool send_event;	   /* true if this came from a SendEvent request */
  Display *display;	   /* Display the event was read from */
  long unused;             /* what port */
  Time time;		   /* milliseconds */
  Atom attribute;          /* atom that identifies attribute */
  long value;              /* value of attribute */
} SVparamChangeEvent;

#endif /* _XLIB_H_ */

typedef struct svideo_node *SV_nodeP;	/* XXX deprecated */
#define svPut24Frame(V, c) svPutFrame(V, c)  /* XXX deprecated */
#define SV_BLANKING_BUFFER_SIZE	SV_PAL_BLANKING_BUFFER_SIZE /* XXX deprecated */

#ifdef __cplusplus
}
#endif
#endif /* !__SVIDEO_H__ */
