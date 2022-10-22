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
*   o  sample rate
*   o  number of channels
*   o  compression (or "coding") scheme
*
* If the data is not compressed (ie, if the "coding scheme" is "linear PCM"),
* an additional parameter describes the numerical format of the samples:
*
*   o  sample format (two's complement, unsigned integer, float, double)
*
* If the numerical format for the linear PCM data is two's 
* complement or unsigned integer, 
* an additional parameter describes the number of bits of resolution:
*
*   o  sample width (number of meaningful bits per sample)
*
* Library Compatibility
* ---------------------
*
*    Each of the digital media libraries supports a different set of
*    audio formats.  There are comments to the right of each constant
*    indicating which of the libraries support it:
*
*            MV = Movie Library
*            AF = Audio File Library
*            CL = Compression Library
*
**********************************************************************/


/********
*
* Sample Data Format:
*	Data format used to represent linear PCM samples.
*
********/

#define DM_AUDIO_FORMAT		"DM_AUDIO_FORMAT"
    
/********
*
* Note that _DMaudioformat values are used by <dmedia/audiofile.h>
* for its AF_SAMPFMT_* values.
*
********/

typedef enum __DMaudioformat
{
    DM_AUDIO_TWOS_COMPLEMENT = 401,			/* MV, AF */
    DM_AUDIO_UNSIGNED        = 402,			/* MV, AF */
    DM_AUDIO_FLOAT           = 403,                     /*     AF */
    DM_AUDIO_DOUBLE          = 404                      /*     AF */
} DMaudioformat;

/********
*
* Sample Resolution in Bits: 
*	Relevant for integer linear PCM data formats (two's complement, signed).
*
********/

#define DM_AUDIO_WIDTH		"DM_AUDIO_WIDTH"

#define DM_AUDIO_WIDTH_8	 8			/* MV */
#define DM_AUDIO_WIDTH_16	 16			/* MV */
#define DM_AUDIO_WIDTH_24	 24


/********
*
* Sampling Rate:  	
*	Stored as a double.
*
********/

#define DM_AUDIO_RATE		"DM_AUDIO_RATE"

/********
*
* Number of Channels:	
*	Stored as an integer.
*
********/

#define DM_AUDIO_CHANNELS	"DM_AUDIO_CHANNELS"

/********
*
* Audio Compression Schemes. Uncompressed audio refers to linear PCM data.
*
********/

#define DM_AUDIO_COMPRESSION	"DM_AUDIO_COMPRESSION"

#define DM_AUDIO_UNCOMPRESSED	"Uncompressed Audio"	/* MV, CL */
#define DM_AUDIO_G711_ULAW	"G.711 u-law"           /* CL */
#define DM_AUDIO_G711_ALAW	"G.711 A-law"           /* CL */
#define DM_AUDIO_MPEG1		"MPEG-1 Audio"          /* CL */
#define DM_AUDIO_MPEG		DM_AUDIO_MPEG1
#define DM_AUDIO_MULTIRATE	"Aware MultiRate"       /* CL */
#define DM_AUDIO_G722		"G.722"
/*
 * SGI does not provide encode/decode support for the following schemes
 */
#define DM_AUDIO_APPLE_ACE2	"Apple ACE2"
#define DM_AUDIO_APPLE_ACE8     "Apple ACE8"
#define DM_AUDIO_APPLE_MAC3     "Apple MAC3"
#define DM_AUDIO_APPLE_MAC6     "Apple MAC6"

/*******
*
* Bitrate
*
********/

#define DM_AUDIO_BITRATE  "DM_AUDIO_BITRATE"

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
				    double     audioSamplingRate,
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
