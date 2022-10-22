/*****************************************************************************
 *
 * SGI movie library 
 *
 * movieplay.h
 *	header file for use with /usr/lib/libmovie.a
 *      movie playback interface
 *
 * Copyright 1992 & 1993, Silicon Graphics, Inc.
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
 *
 ****************************************************************************/

#ifndef __MOVIEPLAY_H__
#define __MOVIEPLAY_H__

#ident "$Revision: 1.10 $"

#include <X11/Xlib.h>		/* for Display, Window */
#include <GL/glx.h>		/* for GLXContext */

#include <dmedia/dmedia.h>
#include <dmedia/dm_audio.h>
#include <dmedia/dm_image.h>
#include <dmedia/dm_timecode.h>
#include <dmedia/dm_params.h>

#ifdef __cplusplus
extern "C" {
#endif

/**********************************************************************
*
* Type Definitions
*
**********************************************************************/

/********
*
* MVeventmask (for mvSetSelectEvents())
*
********/

typedef __uint32_t MVeventmask;

#define MV_EVENT_MASK_FRAME		(1<<1)
#define MV_EVENT_MASK_STOP		(1<<2)
#define MV_EVENT_MASK_ERROR		(1<<3)
#define MV_EVENT_MASK_SLOW_PLAY		(1<<4)

/********
*
* MVeventtype
*
********/

typedef enum __MVeventtype
{
    MV_EVENT_FRAME		= 1,
    MV_EVENT_STOP		= 2,
    MV_EVENT_ERROR		= 3,
    MV_EVENT_SLOW_PLAY		= 4
} MVeventtype;

/********
*
* Playback Events
*
********/

typedef struct __mvFrameEvent {	/* a frame just played */
    MVeventtype		type;	/* event type */
    MVtime 		time;	/* same as X timestamp, in milliseconds */
    MVid 		id;	/* movie instance which produced the event */
    MVframe 		frame;	/* current frame for the movie instance */
} MVframeevent;

typedef struct __mvStopEvent {	/* the movie stopped at the end */
    MVeventtype 	type;
    MVtime 		time;
    MVid 		id;
    MVframe 		frame;
} MVstopevent;

typedef struct __mvErrorEvent {	/* error condition halting playback */
    MVeventtype 	type;
    MVtime 		time;
    MVid 		id;
    MVframe 		frame;
    int 		errcode;
} MVerrorevent;

typedef struct __mvSlowPlayEvent { /* cannot acheive desired frame rate */
    MVeventtype 	type;
    MVtime 		time;
    MVid 		id;
    MVframe 		frame;
    int 		reason;
} MVslowplayevent;

typedef union __mvEvent {
    MVeventtype		type;	    /* common to all events */
    MVframeevent 	mvframe;
    MVstopevent 	mvstop;
    MVerrorevent 	mverror;
    MVslowplayevent 	mvslowplay;
} MVevent;


/**********************************************************************
*
* Playback Functions
*
**********************************************************************/

extern DMstatus	 mvSetNumMoviesHint ( int          numMovies );
extern int       mvGetNumMoviesHint ( void );

extern DMstatus  mvBindOpenGLWindow ( MVid         movieid, 
				      Display*     dpy,
				      Window       win,
				      GLXContext   ctxt );
extern DMstatus  mvUnbindOpenGLWindow( MVid        movie );

extern DMstatus  mvBindWindow       ( MVid         movieid, 
				      Display*     dpy,
				      Window       win );
extern DMstatus  mvUnbindWindow     ( MVid         movie );

extern void      mvSetCurrentFrame  ( MVid         movieid, 
				      MVframe      newframe );
extern MVframe   mvGetCurrentFrame  ( MVid         movieid );
extern void      mvScrubCurrentFrame( MVid         movieid, 
				      MVframe      newframe );
extern void      mvStop             ( MVid         movieid );
extern void      mvPlay             ( MVid         movieid );

extern void	 mvSetStartFrame    ( MVid	   movieid, 
				      MVframe	   startframe );
extern MVframe	 mvGetStartFrame    ( MVid	   movieid );
extern void	 mvSetEndFrame	    ( MVid	   movieid, 
				      MVframe	   endframe );
extern MVframe	 mvGetEndFrame	    ( MVid	   movieid );

extern void      mvSetEnableAudio   ( MVid         movieid, 
				      DMboolean    onoff );
extern DMboolean mvGetEnableAudio   ( MVid         movieid );
extern void      mvSetPrimaryAudio  ( MVid         movieid );
extern MVid      mvGetPrimaryAudio  ( void );

extern void      mvSetPlayLoopMode  ( MVid         movieid, 
				      MVloopmode   newloopmode );
extern MVloopmode
                 mvGetPlayLoopMode  ( MVid         movieid );
extern void      mvSetPlayLoopLimit ( MVid         movieid, 
				      MVframe      newlooplimit );
extern MVframe   mvGetPlayLoopLimit ( MVid         movieid );
extern void      mvSetPlayLoopCount ( MVid         movieid, 
				      MVframe      newloopcount );
extern MVframe   mvGetPlayLoopCount ( MVid         movieid );

extern void      mvSetViewOffset    ( MVid         movieid, 
				      int          offsetx, 
				      int          offsety,
				      DMboolean    glcoordsystem ); 
extern void      mvGetViewOffset    ( MVid         movieid,
				      int*         offsetxreturn,
				      int*         offsetyreturn,
				      DMboolean    glcoordsystem ); 
extern void      mvQueryViewOffset  ( MVid         movieid, 
				      int          offsetx, 
				      int          offsety,
				      int*         offsetxreturn, 
				      int*         offsetyreturn,
				      DMboolean    glcoordsystem );  
extern void      mvSetViewSize      ( MVid         movieid, 
				      int          newwidth, 
				      int          newheight,
				      DMboolean    keepaspect ); 
extern void      mvGetViewSize      ( MVid         movieid, 
				      int*         widthreturn,
				      int*         heightreturn );
extern void      mvQueryViewSize    ( MVid         movieid, 
				      int          width, 
				      int          height,
				      DMboolean    keepaspect, 
				      int*         widthreturn,
				      int*         heightreturn );  
extern void      mvSetViewBackground( MVid         movieid,
				      unsigned short red,
				      unsigned short green,
				      unsigned short blue );
extern void      mvGetViewBackground( MVid         movieid, 
				      unsigned short* redreturn,
				      unsigned short* greenreturn,
				      unsigned short* bluereturn ); 

extern DMstatus	 mvGetEventFD       ( int *fdreturn );
extern void      mvSetSelectEvents  ( MVeventmask  eventmask );
extern MVeventmask
                 mvGetSelectEvents  ( void );
extern void      mvNextEvent        ( MVevent*     eventreturn );
extern void      mvPeekEvent        ( MVevent*     eventreturn );
extern int       mvPendingEvents    ( void );

extern DMstatus	 mvGetActualFrameRate
                                    ( MVid         movieid,
				      double*	   ratereturn );
extern void      mvSetPlaySpeed     ( MVid         movieid, 
				      double       newplayspeed );
extern double    mvGetPlaySpeed     ( MVid         movieid );
extern void      mvSetPlayEveryFrame( MVid         movieid,
				      DMboolean    sync );
extern DMboolean mvGetPlayEveryFrame( MVid         movieid );

extern void      mvSetSlowThreshold ( double       slowthresh );
extern double    mvGetSlowThreshold ( void );

extern void      mvShowCurrentFrame ( MVid         movieid );

extern void      mvResizeOpenGLWindow ( Display*     dpy, 
				      Window       win,
				      GLXContext   ctxt );

extern void      mvResizeWindow     ( Display*     dpy, 
				      Window       win );

extern void      mvSetFrameDisplay  ( DMboolean    showframes );
extern DMboolean mvGetFrameDisplay  ( void );

extern void      mvGrabOpenGL       ( void );
extern void      mvReleaseOpenGL    ( void );

extern void      mvGrabIrisGL       ( void );
extern void      mvReleaseIrisGL    ( void );


#ifdef __cplusplus
}
#endif
#endif /* !__MOVIEPLAY_H__ */
