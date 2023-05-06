/*****************************************************************************
 *
 * SGI movie library 
 *
 * moviefile.h
 *	header file for use with /usr/lib/libmovie.a
 *      movie file I/O interface
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

#ifndef __MOVIEFILE_H__
#define __MOVIEFILE_H__

#ident "$Revision: 1.3 $"

#include <fcntl.h>		/* for O_RDONLY, O_RDWR */

#include <dmedia/dmedia.h>
#include <dmedia/dm_audio.h>
#include <dmedia/dm_image.h>
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
* MVid
*
* Used to identify objects inside the movie library:
*
*   Movies
*   Tracks
*   Images (not yet supported)
*
********/

typedef unsigned long MVid;

/*
** Special value for mvSetPrimaryAudio().  This is a value that
** is never a valid movie ID.
*/

#define MV_PRIMARY_AUDIO_NOTSET 7

/********
*
* MVfileformat
*
* Defines the format of a movie file on the disk (or wherever it may
* be).  
*
********/

typedef enum __MVfileformat
{
    MV_FORMAT_SGI_1	= 0,	/* SGI format -- version 1 */
    MV_FORMAT_SGI_2	= 1,	/* SGI format -- version 2 */
    MV_FORMAT_SGI_3	= 2,	/* SGI format -- version 3 */
    MV_FORMAT_QT	= 3,	/* Apple     QuickTime */
    MV_FORMAT_OMFI	= 4,	/* Avid      OMFI (not supported) */
    MV_FORMAT_AVI	= 5,	/* Microsoft AVI  (not supported) */
    MV_FORMAT_MPEG1	= 6	/* MPEG1 (ISO 11172) (not supported) */
} MVfileformat;

/********
*
* MVloopmode
*
********/

typedef enum __MVloopmode
{
    MV_LOOP_NONE	= 0,
    MV_LOOP_CONTINUOUSLY= 1,
    MV_LOOP_SWINGING	= 2
} MVloopmode;

/*
** The MV_LOOP_COUNT parameter has a special setting that means to
** loop forever (passed into mvSetLoopLimit).
*/

#define MV_LIMIT_FOREVER		0

/********
*
* MVframe
*
********/

typedef long MVframe;

/**********************************************************************
*
* Error Handling Functions
*
**********************************************************************/

extern int         mvGetErrno( void );
extern const char* mvGetErrorStr( int errno );

/**********************************************************************
*
* Movie Functions
*
* A movie is a set of tracks.  The data in the tracks is stored in a
* storage medium (such as a file).
*
* The operations that can be performed on a movie include:
*
*   1) Creating a new movie instance by either opening an existing 
*      file or by creating a new movie file.
*
*   2) Adding, Removing, and Finding tracks.
*
*   3) Getting and setting the properties (tags) of the movie.
*
*   4) Destroying a movie instance, which flush any changes made
*      out to the file.
*
* The settable properties of a movie are:
*
*    * File format (SGI, QuickTime, etc.)
*    * Looping mode and number of loops.
*    * Title, Comment.
*
* The read-only properties of a movie are: 
* (these are computed by the library) 
*
*    * Maximum image width, height, frames-per-second.
*    * Number of image tracks.
*    * Number of audio tracks.
*
**********************************************************************/

extern DMstatus  mvSetMovieDefaults ( DMparams*    params,
				      MVfileformat format );

extern DMboolean mvIsMovieFile      ( const char*  filename );
extern DMstatus  mvCreateFile       ( const char*  filename,
				      DMparams*    params,
				      DMparams*    returnParamsSetOrNull,
				      MVid*        returnMovie );

extern DMstatus  mvOpenFile         ( const char*  filename,
				      int          oflag,
				      MVid*        returnMovie );

extern DMboolean mvIsMovieFD        ( int fd );
extern DMstatus  mvCreateFD         ( int          fd, 
				      DMparams*    params,
				      DMparams*    returnParamsSetOrNull,
				      MVid*        returnMovie );
extern DMstatus  mvOpenFD           ( int          fd,
				      MVid*        returnMovie );

extern DMboolean mvIsMovieMem       ( void*        pointer,
				      size_t       size );
extern DMstatus  mvCreateMem        ( void*        pointer, 
				      size_t	   size,
				      DMparams*    params,
				      DMparams*    returnParamsSetOrNull,
				      MVid*        returnMovie );
extern DMstatus  mvOpenMem          ( void*        pointer, 
				      size_t       size,
				      MVid*        returnMovie );

extern DMstatus  mvClose            ( MVid         movie );

extern DMstatus  mvWrite	    ( MVid         movie );

extern DMstatus  mvAddTrack         ( MVid         movie,
				      DMmedium     type,
				      DMparams*    params,
				      DMparams*    returnParamsSetOrNull,
				      MVid*        returnTrack );

extern DMstatus  mvRemoveTrack      ( MVid         movie,
				      MVid         track );

extern DMstatus  mvFindTrackByMedium( MVid         movie,
				      DMmedium     type,
				      MVid*        returnTrack );

extern DMstatus  mvAddUserParam     ( const char*  paramName );

extern DMparams* mvGetParams        ( MVid         movieOrTrack );

extern DMstatus  mvSetParams        ( MVid         movieOrTrack,
				      DMparams*    params,
				      DMparams*    returnParamsSetOrNull );

extern DMstatus  mvOptimize         ( MVid         fromMovie, 
				      MVid         toMovie);
                        

/**********************************************************************
*
* Track Functions
*
* The only kind of track that we support now is an evenly spaced (in
* time) sequence of frames, where each of the frames is the same size.
* (Although the size may vary after compression.)
*
* The operations that can be performed on a track include:
*
*   1) Writing frames (either inserting or overwriting).
*
*   2) Reading frames
*
*   3) Editing operations (copying from one movie to another).  These
*      are equivalent to reading and then writing but may be
*      implemented more efficiently.)
*
* The settable properties of all tracks are:
*
*    * Compression scheme
*    * Frame rate
*    * Frame format
*    * SMPTE stuff (not supported yet)
*
* The read-only properties of all tracks are:
*
*    * Length (number of frames)
*    * Frame size (in bytes)
*    * Track type (image or audio)
*
* The settable properties of an image track are:
*
*    * Size (width and height)
*
* The settable properties of an audio track are:
*
*    * Frame size
*    * Number of channels
*
**********************************************************************/

/********
*
* Data formats
*
********/

extern DMstatus  mvMapBetweenTracks ( MVid         fromTrack,
				      MVid         toTrack,
				      MVframe      fromFrameIndex,
				      MVframe*     returnToFrameIndex );

/********
*
* Reading and Writing
*
********/

extern DMstatus  mvReadFrames       ( MVid         track,
				      MVframe      frameIndex,
				      MVframe      frameCount,
				      size_t       buffer_size,
				      void*        buffer );

extern DMstatus  mvInsertFrames     ( MVid         track,
				      MVframe      frameIndex,
				      MVframe      frameCount,
				      size_t       buffer_size,
				      void*        buffer );

extern DMstatus  mvDeleteFrames     ( MVid         track,
				      MVframe      frameIndex,
				      MVframe      frameCount );
				     
extern DMstatus  mvPasteFrames      ( MVid         fromTrack,
				      MVframe      fromFrameIndex,
				      MVframe      fromFrameCount,
				      MVid         toTrack,
				      MVframe      toFrameIndex );
				     
/********
*
* Direct Access to Compressed Images
*
********/

extern size_t    mvGetCompressedImageSize
                                    ( MVid         track,
				      MVframe      frameIndex );
				      
extern DMstatus  mvInsertCompressedImage
                                    ( MVid         track,
				      MVframe      frameIndex,
				      size_t       compressed_size,
				      void*        buffer );

extern DMstatus  mvReadCompressedImage
                                    ( MVid         track,
				      MVframe      frameIndex,
				      size_t       buffer_size,
				      void*        buffer );

/**********************************************************************
*
* Properties of Movies and Tracks
*
* These macros allow easy access to the parameters for a movie or
* track.  They can also be obtained by getting the parameter list
* with mvGetParams and then using the dmParamsGet functions.
*
* These all apply to the format of the data stored in the movie,
* *not* to the virtual format of a track.
*
**********************************************************************/

/********
*
* Movie Properties
*
********/

MVfileformat mvGetFileFormat       ( MVid movie );
MVloopmode   mvGetLoopMode         ( MVid movie );
int          mvGetLoopLimit        ( MVid movie );
const char*  mvGetTitle            ( MVid movie );
const char*  mvGetComment          ( MVid movie );
DMboolean    mvGetOptimized        ( MVid movie );

DMstatus     mvSetLoopMode         ( MVid movie, MVloopmode  mode    );
DMstatus     mvSetLoopLimit        ( MVid movie, int         limit   );
DMstatus     mvSetTitle            ( MVid movie, const char* title   );
DMstatus     mvSetComment          ( MVid movie, const char* comment );

/********
*
* Generic Track Properties
*
********/

int          mvGetTrackLength      ( MVid track );
DMmedium     mvGetTrackMedium      ( MVid track );
const char*  mvGetTrackSMPTEStart  ( MVid track );

DMstatus     mvSetTrackSMPTEStart  ( MVid track, const char* timecode );

/********
*
* Image Track Properties
*
********/

int           mvGetImageWidth      ( MVid imageTrack );
int           mvGetImageHeight     ( MVid imageTrack );
double        mvGetImageRate       ( MVid imageTrack );
const char*   mvGetImageCompression( MVid imageTrack );
DMinterlacing mvGetImageInterlacing( MVid imageTrack );
DMpacking     mvGetImagePacking    ( MVid imageTrack );
DMorientation mvGetImageOrientation( MVid imageTrack );

DMstatus      mvSetImageRate       ( MVid imageTrack, double rate );
						    
/********
*
* Audio Track Properties
*
********/

DMaudioformat mvGetAudioFormat     ( MVid audioTrack );
int           mvGetAudioWidth      ( MVid audioTrack );
double        mvGetAudioRate       ( MVid audioTrack );
int           mvGetAudioChannels   ( MVid audioTrack );
const char*   mvGetAudioCompression( MVid audioTrack );
double        mvGetDefaultVol      ( MVid audioTrack );

DMstatus      mvSetDefaultVol      ( MVid audioTrack, double vol );

/**********************************************************************
*
* Error codes
*
* Most Movie Library calls return DM_SUCCESS on success, DM_FAILURE
* on failure. mvGetErrno() returns the last movielib error.
*
* We include our own error codes for things which will be
* caught by the regular errno.  However, they are the same
* numerical values as those assigned to the Unix system errno.
*
**********************************************************************/

#define MV_ARENA_FULL	12			/* shared arena for synchronizing
						 * movie tasks is full (ENOMEM) */
#define MV_NO_PROCESS	11			/* no more entries in process
						 * table for movie tasks (EAGAIN) */

#define MV_EBASE		1000

#define MV_BAD_ID		( 1+MV_EBASE) /* "Bad movie or track ID" */
#define MV_NO_RGB		( 2+MV_EBASE) /* "This machine does not support GL RGB mode. Cannot display movies" */
#define MV_BAD_FORMAT           ( 3+MV_EBASE) /* "File is not a movie file or is an unsupported format" */
#define MV_BAD_MALLOC           ( 4+MV_EBASE) /* "Unable to allocate memory" */
#define MV_BAD_GRAB_GL_NESTING  ( 5+MV_EBASE) /* "mvGrabIrisGL()/mvReleaseIrisGL() called out of order" */
#define MV_READWRITE_REQUIRED   ( 6+MV_EBASE) /* "Editing a movie requires write access to the file" */
#define MV_QT_UNSUPPORTED	( 7+MV_EBASE) /* "Unsupported QuickTime(tm) Feature: %s" */
#define MV_INTERNAL_ERROR	( 8+MV_EBASE) /* "Internal movie library error" */
#define MV_NO_TRACK             ( 9+MV_EBASE) /* "No track of requested type" */
#define MV_BAD_TRACK_MEDIUM     (10+MV_EBASE) /* "Only audio and image tracks are supported" */
#define MV_BAD_FRAME_NUMBER     (11+MV_EBASE) /* "Frame number is out of range for track" */
#define MV_BAD_COMP_TYPE        (12+MV_EBASE) /* "Unknown image compression scheme" */
#define MV_UNSUP_COMP_TYPE	(13+MV_EBASE) /* "Unsupported image compression scheme" */
#define MV_FRAME_SIZE_MISMATCH  (14+MV_EBASE) /* "Frame sizes not the same when copying between tracks" */
#define MV_MOVIE_NOT_EMPTY  	(15+MV_EBASE) /* "Destination movie in mvOptimize is not empty" */
#define MV_QT_NO_IMAGE		(16+MV_EBASE) /* "A QuickTime(tm) movie must have an image track" */
#define MV_NO_QUICKTIME		(17+MV_EBASE) /* "QuickTime(tm) codecs (libqt) not present" */
#define MV_BAD_AUDIO_PARAMS	(18+MV_EBASE) /* "Bad settings for audio track" */
#define MV_BAD_IMAGE_PARAMS	(19+MV_EBASE) /* "Bad settings for image track" */
#define MV_BAD_MOVIE_PARAMS	(20+MV_EBASE) /* "Bad settings for movie" */
#define MV_MEM_TOO_SMALL     	(21+MV_EBASE) /* "Space for in-memory movie is too small" */
#define MV_CL_ERROR             (22+MV_EBASE) /* "Error in compression library: %s" */
#define MV_BAD_IMAGE_FORMAT     (23+MV_EBASE) /* "Image format not supported for playback" */
#define MV_USER_PARAM_TYPE      (24+MV_EBASE) /* "Values for user parameters must be strings: %s" */
#define MV_PARAM_TYPE           (25+MV_EBASE) /* "Parameter value supplied is of the wrong type: %s" */

/**********************************************************************
*
* Parameter Name Constants
*
**********************************************************************/

/********
*
* Movie Tags
*
********/

#define MV_FILE_FORMAT          "FILE_FORMAT"
#define MV_LOOP_LIMIT           "LOOP_LIMIT"
#define MV_LOOP_MODE            "LOOP_MODE"
#define MV_TITLE                "TITLE"
#define MV_COMMENT              "COMMENT"
#define MV_OPTIMIZED		"OPTIMIZED"

/********
*
* Generic Track Tags
*
********/

#define MV_TRACK_LENGTH		"TRACK_LENGTH"
#define MV_SMPTE_START		"SMPTE_START"

/********
*
* Audio Track Tags
*
********/

#define MV_DEFAULT_VOL          "DEFAULT_VOL"


#ifdef __cplusplus
}
#endif
#endif /* !__MOVIEFILE__H__ */
