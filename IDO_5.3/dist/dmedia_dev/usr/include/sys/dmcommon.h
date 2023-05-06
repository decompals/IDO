#ifndef _SYS_DMCOMMON_H_
#define _SYS_DMCOMMON_H_
/**************************************************************************
 *                                                                        *
 *               Copyright (C) 1990, Silicon Graphics, Inc.               *
 *                                                                        *
 *  These coded instructions, statements, and computer programs  contain  *
 *  unpublished  proprietary  information of Silicon Graphics, Inc., and  *
 *  are protected by Federal copyright law.  They  may  not be disclosed  *
 *  to  third  parties  or copied or duplicated in any form, in whole or  *
 *  in part, without the prior written consent of Silicon Graphics, Inc.  *
 *                                                                        *
 **************************************************************************/
/*
 * dmcommon.h
 *
 * $Revision$
 *
 * Header information for the ringbuffers that may be required in the
 * kernel.
 */

#include <sys/time.h>

/*
 * following are general dmedia header structures and the tags that
 * should be associated with them.
 */

#define DM_RB_DMEDIA_INFO	0x100001
#define DM_RB_DMEDIA_TAG_NAME	"DMediaInfoStructure"

#define DM_RB_TYPE_IMAGE	1001	/* image data is in this buffer */
#define DM_RB_TYPE_AUDIO	1002	/* audio data is in this buffer */
#define DM_RB_TYPE_VIDEO	1003	/* both audio and video are here */

typedef struct DMediaInfo
{
    int type;			/* should be enum? */
    struct timeval time;	/* the system time */
    struct timeval duration;	/* the duration of this buffer */
    int sequence;		/* field sequence number */
    unsigned long long ustime;	/* unadjusted system time in nanoseconds */
} DMediaInfo;

#define DM_RB_IMAGE_INFO	0x100002
#define DM_RB_IMAGE_TAG_NAME	"DMImageInfoStructure"

typedef struct DMImageInfo
{
    int offset;			/* offset to start of data */
    int width, height;		/* the size of the image */
    int packing;		/* the pixel packing format */
    int compression;		/* how the data is compressed */
} DMImageInfo;

#define DM_RB_AUDIO_INFO	0x100003
#define DM_RB_AUDIO_TAG_NAME	"DMAudioInfoStructure"

typedef struct DMAudioInfo
{
    int offset;			/* offset to start of data */
    int width;			/* the sample width */
    int format;			/* the sample format */
    double rate;		/* the sampling rate */
    int compression;		/* the compression format */
} DMAudioInfo;

#define DMImageAudio DMAudioInfo	/* oops, a typo got into vl_1.0, */
					/* keep reference for compatibility */
#endif /* _SYS_DMCOMMON_H */
