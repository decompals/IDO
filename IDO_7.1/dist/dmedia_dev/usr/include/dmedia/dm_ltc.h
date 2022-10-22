#ifndef __DM_LTC_H__
#define __DM_LTC_H__

/***************************************************************************
 * SGI Digital Media Library: LTC Utility Routines
 *
 * <dmedia/dm_ltc.h>
 *    header file for use with /usr/lib/libdmedia.so
 ***************************************************************************
 * 
 * Copyright 1995, Silicon Graphics, Inc.
 * ALL RIGHTS RESERVED
 *
 * UNPUBLISHED -- Rights reserved under the copyright laws of the United
 * States.   Use of a copyright notice is precautionary only and does not
 * imply publication or disclosure.
 *
 * U.S. GOVERNMENT RESTRICTED RIGHTS LEGEND:
 * Use, duplication or disclosure by the Government is subject to restrictions
 * as set forth in FAR 52.227.19(c)(2) or subparagraph (c)(1)(ii) of the Rights
 * in Technical Data and Computer Software clause at DFARS 252.227-7013 and/or
 * in similar or successor clauses in the FAR, or the DOD or NASA FAR
 * Supplement.  Contractor/manufacturer is Silicon Graphics, Inc.,
 * 2011 N. Shoreline Blvd. Mountain View, CA 94039-7311.
 *
 * THE CONTENT OF THIS WORK CONTAINS CONFIDENTIAL AND PROPRIETARY
 * INFORMATION OF SILICON GRAPHICS, INC. ANY DUPLICATION, MODIFICATION,
 * DISTRIBUTION, OR DISCLOSURE IN ANY FORM, IN WHOLE, OR IN PART, IS STRICTLY
 * PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN PERMISSION OF SILICON
 * GRAPHICS, INC.
 *
 ****************************************************************************/

#include <dmedia/dm_timecode.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _DMLTCcode {
    DMtimecode		tc;		/* Timecode */
    unsigned int	dropFrame :1;	/* NTSC drop frame mode on */
    unsigned int	colorLock :1;	/* Count locked to color framing */
    char		userType;	/* Binary Group flags for user data */
    char		userData[4];	/* User Group bytes */
} DMLTCcode;

typedef struct _DMLTCdecoder *DMLTCdecoder;

extern DMstatus	dmLTCDecoderCreate(DMLTCdecoder *decoder,
					int timecodeType);

extern DMstatus	dmLTCDecoderDestroy(DMLTCdecoder decoder);

extern DMstatus	dmLTCDecode(DMLTCdecoder decoder,
					void **sampleBuffer,
					int *sampleCount,
					DMLTCcode *dmLTCcodeword );

extern DMstatus	dmLTCDecoderSetParams(DMLTCdecoder decoder,
					DMparams *audioParams,
					int timecodeChannel);
#ifdef __cplusplus 
}
#endif

#endif /* __DM_LTC_H__ */
