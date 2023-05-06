#ifndef __INC_DM_AUDIO_H__
#define __INC_DM_AUDIO_H__  

/*****************************************************************************
*
*  Copyright 1993, Silicon Graphics, Inc.
*  All Rights Reserved.
*
*  This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
*  the contents of this file may not be disclosed to third parties, copied or
*  duplicated in any form, in whole or in part, without the prior written
*  permission of Silicon Graphics, Inc.
*
*  RESTRICTED RIGHTS LEGEND:
*  Use, duplication or disclosure by the Government is subject to restrictions
*  as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
*  and Computer Software clause at DFARS 252.227-7013, and/or in similar or
*  successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
*  rights reserved under the Copyright Laws of the United States.
* 
*****************************************************************************/

#include <stdlib.h>		/* for size_t */
#include <dmedia/dm_params.h>

#ifdef __cplusplus
extern "C" {
#endif

/**********************************************************************
*
* Audio Parameters
* ----------------
*
* The following set of parameters defines how audio data is represented:
*
*   * Sample Width (number of bits per sample)
*   * Sample Format (e.g., twos-complement binary, floating point)
*   * Float min/max
*   * Sample Rate
*   * Number of Channels
*   * Compression Scheme
*
* Library Compatibility
* ---------------------
*
*    Each of the digital media libraries supports a different set of
*    audio formats.  There are comments to the right of each constant
*    indicating which of the libraries support it:
*
*            MV = Movie Library
*
**********************************************************************/

/********
*
* Sample Width
*
********/

#define DM_AUDIO_WIDTH		"DM_AUDIO_WIDTH"

#define DM_AUDIO_WIDTH_8	 8			/* MV */
#define DM_AUDIO_WIDTH_16	 16			/* MV */
#define DM_AUDIO_WIDTH_24	 24

/********
*
* Sample Format
*
********/

#define DM_AUDIO_FORMAT		"DM_AUDIO_FORMAT"
    
typedef enum __DMaudioformat
{
    DM_AUDIO_TWOS_COMPLEMENT = 401,			/* MV */
    DM_AUDIO_UNSIGNED        = 402,			/* MV */
    DM_AUDIO_FLOAT           = 403,
    DM_AUDIO_DOUBLE          = 404
} DMaudioformat;

/********
*
* Float min/max - doubles
*
********/

/********
*
* Sample Rate - Stored as a floating point number (double)
*
********/

#define DM_AUDIO_RATE		"DM_AUDIO_RATE"

/********
*
* Number of channels - int
*
********/

#define DM_AUDIO_CHANNELS	"DM_AUDIO_CHANNELS"

/********
*
* Compression Scheme
*
********/

#define DM_AUDIO_COMPRESSION	"DM_AUDIO_COMPRESSION"

#define DM_AUDIO_UNCOMPRESSED	"Uncompressed Audio"	/* MV */
#define DM_AUDIO_G711_ULAW	"G.711 u-law"
#define DM_AUDIO_G711_ALAW	"G.711 A-law"
#define DM_AUDIO_MPEG1		"MPEG1Audio"
#define DM_AUDIO_MPEG		DM_AUDIO_MPEG1
#define DM_AUDIO_MULTIRATE	"Aware MultiRate"
#define DM_AUDIO_G722		"G.722"

/**********************************************************************
*
* Audio Functions
*
**********************************************************************/

/********
*
* dmSetAudioDefaults
*
********/

extern DMstatus dmSetAudioDefaults( DMparams*  toParams,
				    int        audioWidth,
				    double     audioRate,
				    int        audioChannels );

/********
*
* dmAudioFrameSize
*
* Returns the number of bytes required to store an audio frame.
* (One sample for all channels).
*
********/

size_t dmAudioFrameSize( const DMparams* params );

#ifdef __cplusplus 
}
#endif

#endif /* ! __INC_DM_AUDIO_H__  */


